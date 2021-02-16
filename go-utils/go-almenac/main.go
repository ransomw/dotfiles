package main

/**
 * todo
 *
 * - broadcast to
 *   o X/Wayland desktop (zsh client)
 *   o audio player (zsh client)
 *     ..incl. xmessage popups here
 *   o daily "reciept" of appts., directions,
 *     agenda, etc.
 *     printed to a literal receipt printer.
 *     if such can be acquired.
 *   in yamldb wherever possible.
 *
 * - go-almenac remote add travel goa://lisa/personal/travel
 *   -- i.e. git-like semantics for managing multiple
 *
 * - store creds (gcal token and .json download after first use)
 *      yaml data stores, local or remote.
 */

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"math/rand"
	"net/http"
	"os"
	"os/user"
	"path/filepath"
	"reflect"
	"strconv"
	"strings"
	"text/template"
	"time"

	yaml "gopkg.in/yaml.v2"
	uuid "github.com/satori/go.uuid"
	gauth "golang.org/x/oauth2/google"
	oauth "golang.org/x/oauth2"
	cal "google.golang.org/api/calendar/v3"
)

const (
	YamlDBEntry = iota
)

const (
	YamlDBPath = "yamldb"
)

// the ftp connection provides a flat list of filenames,
// and files can be added to it from specified paths.
// all filenames are uuids.
//
// clients get ftp path info, responsible for bytes upload themselves,
// and may specify files by relative path on the server host if FsPath set.
type FtpConn struct{
	FtpPort int
	FtpHost string
	FtpPath string
	FsPath string
	connected bool
}


func (conn *FtpConn) Connected() bool {
	return conn.connected
}

func newFtpConn(args... interface{}) (conn *FtpConn) {
	conn = new(FtpConn)
	if len(args) < 3 {
		conn.connected = false
		return
	}
	var i interface{}
	i = args[0]
	conn.FtpPort = i.(int)
	i = args[1]
	conn.FtpHost = i.(string)
	i = args[2]
	conn.FtpPath = i.(string)
	return
}

func (conn *FtpConn) Connect() (err error) {
	return errors.New("FtpConn.Connect() unimpl")
	if !conn.connected {
		return errors.New("connection failed")
	}
	return nil
}

func (conn *FtpConn) OpenFile(path interface{}) (f *os.File, err error) {
	id, ok := path.(uuid.UUID)
	if !ok {
		ps := path.(string)
		id, err = uuid.FromString(ps)
		if err != nil { return nil, err }
	}
	return os.Open(filepath.Join(conn.FsPath, id.String()))
}

func (conn *FtpConn) AddFile(f *os.File) (uuid.UUID, error) {
	id:= uuid.NewV1()

	fout, err := os.OpenFile(
		filepath.Join(conn.FsPath, id.String()),
		os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0577)
	if err != nil { return id, err }
	defer func() { fout.Close() }()

	_, err = io.Copy(fout, f)
	if err != nil { return id, err }

	return id, nil
}

func (conn *FtpConn) listFiles() ([]string, error) {
	rv := make([]string, 0)
	// unimpl: list files using ftp connection rather than file path
	if conn.FsPath == "" { return nil, nil }

	files, err := ioutil.ReadDir(conn.FsPath)
	if err != nil { return nil, err }

	for _, file := range files {
		rv = append(rv, file.Name())
	}
	return rv, nil
}

func (conn *FtpConn) ListFiles() ([]uuid.UUID, error) {
	ids := make([]uuid.UUID, 0)
	files, err := conn.listFiles()
	if err != nil { return nil, err }
	for _, name := range files {
		id, err := uuid.FromString(name)
		if err != nil { continue }
		ids = append(ids, id)
	}
	return ids, nil
}

var ftpConn *FtpConn

func NewFtpConn(paths... string) (*FtpConn, error) {
	var path string
	if len(paths) < 1 {
		path = "go-almenac-afs-00"
	} else {
		path = paths[0]
	}
	conn := newFtpConn(0, "localhost", path)
	if len(paths) > 1 {
		conn.FsPath = paths[1]
	} else {
		conn.FsPath = "/home/ftp/path/" + path
	}
	conn.Connect()
	if conn.Connected() {
		return conn, nil
	}
	return nil, errors.New("failed to connect to ftp")
}


type Tag string

type Item struct{
	Name string `yaml: "name"`
	Id string `yaml: "id"`
// RFC5545 is "the" unifing standard for ids between events
// created here, in gcal, or elsewhere.
	GCalId string `yaml: "id-gcal"`
	Path string `yaml: "path"`
	file *os.File
	Tags []Tag `yaml: "tags"`
	Start time.Time `yaml: "start"`
	End time.Time `yaml: "end"`
	AddTime time.Time `yaml: "add-time"`
	Detail string `yaml: "detail"`
}

type WebHistory struct{
	Name string `yaml: "pageName""`
	URL string `yaml: "url"`
	Time time.Time `yaml: "time"`
}

type Bookmark struct{
	Name string `yaml: "pageOrBookmarkName""`
	URL string `yaml: "url"`
	Path string `yaml: "path"` // /-delimited
}

type YamlDB struct{
	Items []Item `yaml: "items"`
	ftpConn *FtpConn
}

type ModYamlDBFn func(yamlDB *YamlDB) (err error)

// *not* thread safe.  verify that no two parts of the program use unsafely.
func WithYamlDB(modYamlDbFn ModYamlDBFn) (err error) {
	var yamlDb YamlDB
	yamlDb.ftpConn = ftpConn

	f, err := os.Open(YamlDBPath)
	if os.IsNotExist(err) {
		fmt.Println("file at '" + YamlDBPath + "' doesn't exist already")
		yamlDb.Items = make([]Item, 0)
		f, err = os.Create(YamlDBPath)
		if err != nil { return }
	} else if err != nil {
		return
	} else {
		var by []byte
		by, err = ioutil.ReadAll(f)
		if err != nil { return }
		err = yaml.Unmarshal(by, &yamlDb)
		if err != nil { return }
	}

	existingItemUuids := make(map[string]bool)
	for _, item := range yamlDb.Items {
		existingItemUuids[item.Id] = true
	}
	files, err := yamlDb.ftpConn.ListFiles()
	if err != nil { return }
	for _, file := range files {
		uuidString := file.String()
		if !existingItemUuids[uuidString] {
			yamlDb.Items = append(yamlDb.Items, Item{
				Name: fmt.Sprintf("auto-add uuid %q from ftp filename", uuidString),
				Id: uuidString,
			})
		}
	}

	// timeout on modYamlDbFn call?
	err = modYamlDbFn(&yamlDb)
	if err != nil {
		return
	}

	// validation routines
	//  (start times have end times)
	// go here

	by, err := yaml.Marshal(yamlDb)
	if err != nil { return }
	_, err = f.Seek(0, 0)
	if err != nil { return }
	err = f.Truncate(0)
	if err != nil { return }
	_, err = f.Write(by)
	if err != nil { return }
	return
}

func ModYamlDbAddItem(item Item) ModYamlDBFn {
	var setupErr error
	if item.Tags == nil {
		item.Tags = make([]Tag, 0)
	}
	addFileC := make(chan struct{})
	item.AddTime = time.Now()
	addFileToFtp := func(ftpConn *FtpConn) {
		defer close(addFileC)
		if ftpConn != nil && item.file != nil {
			if !ftpConn.Connected() {
				fmt.Println("ftp connected not connected")
				return
			}
			uuid, setupErr := ftpConn.AddFile(item.file)
			if setupErr != nil { return }
			item.Id = uuid.String()
		}
		if item.Id == "" {
			item.Id = uuid.NewV1().String()
		}
	}
	return func(yamlDB *YamlDB) (err error) {
		if item.file != nil { addFileToFtp(yamlDB.ftpConn) }
		for _ = range addFileC {}
		if setupErr != nil { err = setupErr; return }
		yamlDB.Items = append(yamlDB.Items, item)
		return
	}
}

type ViewCalculation func (YamlDB) interface{}

func ModYamlDbCerialized(calcRes ViewCalculation, ts string,
				args... interface{}) ModYamlDBFn {
	t := template.Must(template.New("dbViewTemplate").Parse(ts))
	var out io.Writer
	var setupErr error
	for _, arg := range args {
	switch v := arg.(type) {
	case io.Writer:
		if out == nil { out = v }
	default:
		setupErr = errors.New("unknown cerial arg type")
	}
	}
	if out == nil { out = os.Stdout }
	return func(yamlDB *YamlDB) (err error) {
		if setupErr != nil {
			err = setupErr
			return
		}
		res := calcRes(*yamlDB)
		err = t.Execute(out, res)
		if err != nil { return }
		return
	}
}

var modYamlDbMultLinePlainCerial ModYamlDBFn


func init() {
	modYamlDbMultLinePlainCerial = ModYamlDbCerialized(
		func(yamlDb YamlDB) interface{} {
			return yamlDb
		},
		`{{range .Items}}
{{.Id}} {{.Name}}
{{else}}
no items in db
{{end}}`)
}

type CLIVar struct{
	IntVal int
	StrVal string
	FileVal *os.File
	VarName string
	coerce func(s string) (interface{}, error)
}

func (cliVar *CLIVar) Coerce(s string) (err error) {
	rv, err := cliVar.coerce(s)
	if err != nil { return }
	switch v := rv.(type) {
	case os.File:
		cliVar.FileVal = &v
	case int:
		cliVar.IntVal = v
	case string:
		cliVar.StrVal = v
	default:
		return errors.New("unknown cli var type from coerce()")
	}
	return
}

func (cliVar *CLIVar) String() string {
	if cliVar.FileVal != nil {
		return fmt.Sprintf("<CLIVar<file>: %v>",
			cliVar.FileVal.Name())
	}
	if cliVar.StrVal != "" {
		return fmt.Sprintf("<CLIVar<string>: %q>",
			cliVar.StrVal)
	}
	return "<CLIVar<int or unset>: " + strconv.Itoa(cliVar.IntVal) + ">"
}

type CLIVars struct{
	Name *CLIVar
	Tag *CLIVar
	Year *CLIVar
	Month *CLIVar
	Day *CLIVar
	Hours *CLIVar
	Quarters *CLIVar
	Path *CLIVar
	adHoc map[string]string
}

func (cV *CLIVars) BuiltinNames() ([]string) {
	if cV.adHoc == nil { cV.adHoc = make(map[string]string) }
	cVval := reflect.ValueOf(cV)
	names := make([]string, 0)
	for i := 0; i < cVval.NumField(); i++ {
		cliVar, ok := cVval.Field(i).Interface().(CLIVar)
		if !ok { continue }
		names = append(names, cliVar.VarName)
	}
	return names
}


func (cV *CLIVars) Set(name, val string) error {
	cVval := reflect.ValueOf(cV)
	for i := 0; i < cVval.NumField(); i++ {
		cliVar, ok := cVval.Field(i).Interface().(CLIVar)
		if !ok { continue }
		if cliVar.VarName == name {
			return cliVar.Coerce(val)
		}
	}
	cV.adHoc[name] = val
	return nil
}


// Get with default
func (cV *CLIVars) Get(name, dVal string) (string) {
	cVval := reflect.ValueOf(cV)
	for i := 0; i < cVval.NumField(); i++ {
		cliVar, ok := cVval.Field(i).Interface().(CLIVar)
		if !ok { continue }
		if cliVar.VarName == name {
			return cliVar.String()
		}
	}
	var (
		ok bool
		val string
	)
	val, ok = cV.adHoc[name]
	if ok { return "" }
	return val
}

func NewCLIVars() CLIVars {
var cliVars CLIVars
	coerceName := func(s string) (v interface{}, err error) {
		return s, nil
	}
	coerceTag := func(s string) (v interface{}, err error) {
		return s, nil
	}
	coerceYear := func(s string) (v interface{}, err error) {
		return strconv.Atoi(s)
	}
	coerceMonth := func(s string) (v interface{}, err error) {
		var n int
		n, err = strconv.Atoi(s)
		if err != nil { return }
		if n < 1 || n > 12 {
			return nil, errors.New("months between 1 and 12")
		}
		return n, nil
	}
	coerceDay := func(s string) (v interface{}, err error) {
		var n int
		n, err = strconv.Atoi(s)
		if err != nil { return }
		if n < 1 || n > 31 {
			return nil, errors.New("days between 1 and 31")
		}
		return n, nil
	}
	coerceHours := func(s string) (v interface{}, err error) {
		var n int
		n, err = strconv.Atoi(s)
		if err != nil { return }
		if n < 0 || n > 23 {
			return nil, errors.New("hours between 0 and 23")
		}
		return n, nil
	}
	coerceQuarters := func(s string) (v interface{}, err error) {
		var n int
		n, err = strconv.Atoi(s)
		if err != nil { return }
		if n < 0 || n > 3 {
			return nil, errors.New("quarters of the hour between 0 and 3")
		}
		return n, nil
	}
	coercePath := func(s string) (v interface{}, err error) {
		v, err = os.Open(filepath.Join(ftpConn.FsPath, s))
		if err != nil { return }
		v, err = os.Open(s)
		return
	}
	cliVars = CLIVars{
		Name: &CLIVar{ VarName: "name", coerce: coerceName },
		Tag: &CLIVar{ VarName: "tag", coerce: coerceTag },
		Year: &CLIVar{ VarName: "year", coerce: coerceYear },
		Month: &CLIVar{ VarName: "month", coerce: coerceMonth },
		Day: &CLIVar{ VarName: "day", coerce: coerceDay },
		Hours: &CLIVar{ VarName: "hours", coerce: coerceHours },
		Quarters: &CLIVar{ VarName: "quarters", coerce: coerceQuarters },
		Path: &CLIVar{ VarName: "path", coerce: coercePath },
	}
	return cliVars
}

type CliEnv struct{
	Vars CLIVars
}

func NewCliEnv() (*CliEnv) {
	env := new(CliEnv)
	env.Vars = NewCLIVars()
	return env
}

type CliFn func(env *CliEnv) (*CliEnv, error)

// the cli maintains the meta-knowledge graph.
// it's an interpreted language, not a cli.
var cliFns map[string]CliFn

func init() {
	cliFns = make(map[string]CliFn)
	// todo: write in function
	/*
	cliFns["?"] = helpFn
	cliFns["help"] = helpFn
	*/

	var varsFn CliFn
	varsFn = func(env *CliEnv) (*CliEnv, error) {
		for _, cvName := range env.Vars.BuiltinNames() {
			fmt.Printf("%q = %q\n", cvName, env.Vars.Get(cvName, ""))
		}
		fmt.Println("----")
		for k, v := range env.Vars.adHoc {
			fmt.Printf("%q = %q\n", k, v)
		}
		return env, nil
	}
	cliFns["vars"] = varsFn
	cliFns["v"] = varsFn

	// todo: write in function
	/*
	var tagItemFn CliFn
	tagItemFn = func(env *CliEnv) (*CliEnv, error) { }
	cliFns["tag-item"] = tagItemFn
	cliFns["tag"] = tagItemFn
	cliFns["t"] = tagItemFn
	*/
	var addItemFn CliFn
	addItemFn = func(env *CliEnv) (*CliEnv, error) {
		item := Item{
			Name: env.Vars.Name.StrVal,
			// todo: fill in all fields from environment variables
		}
		if err := WithYamlDB(ModYamlDbAddItem(item)); err == nil {
			return nil, err
		}
		return env, nil
	}
	cliFns["add-item"] = addItemFn
	cliFns["add"] = addItemFn
	cliFns["a"] = addItemFn
	var listItemsFn CliFn
	listItemsFn = func(env *CliEnv) (*CliEnv, error) {
		if err := WithYamlDB(
			modYamlDbMultLinePlainCerial,
		); err == nil {
			return nil, err
		}
		return env, nil
	}
	cliFns["list-items"] = listItemsFn
	cliFns["list"] = listItemsFn
	cliFns["l"] = listItemsFn
	// .. they're all "fire and forget" side-effects-wise
	// with atomic transforms of db.

}

// operating assumption:  one interactive CLILang
// environment per process.
var cliEnv *CliEnv
func init() {
	cliEnv = NewCliEnv()
}

type CliStstmnt interface{
	StstmntT() string // "fn", "var"
	FnNm() string
	VarSetting() (string, string)
}

type CliStstmntLn_00 string

func (csls CliStstmntLn_00) StstmntT() string {
	ss := string(csls)
	if strings.Contains(ss, "=") {
		return "var"
	}
	return "fn"
}

func (csls CliStstmntLn_00) FnNm() string {
	ss := string(csls)
	return strings.TrimSpace(ss)
}

func (csls CliStstmntLn_00) VarSetting() (string, string) {
	ss := string(csls)
	sArr := strings.SplitAfterN(
		strings.TrimSpace(ss),
		"=", 2)
	return sArr[0], sArr[1]
}

func handleStatement(ststmnt CliStstmnt) error {
	var cliFn CliFn
	var ok bool
	switch ststmnt.StstmntT() {
	case "var":
		if err := cliEnv.Vars.Set(ststmnt.VarSetting()); err != nil {
			return fmt.Errorf("error %v <%T> setting variable",
				err, err)
		}
	case "fn":
		fnNm := ststmnt.FnNm()
		cliFn, ok = cliFns[fnNm]
		if !ok { return fmt.Errorf("unknown function name %q", fnNm) }
		if updatedCliEnv, err := cliFn(cliEnv); err != nil {
			return fmt.Errorf("error %v <%T> executing %q",
				err, err, fnNm)
		} else {
			cliEnv = updatedCliEnv
		}
	default:
		return errors.New("unknown statement type")
	}
	return nil
}


const (
	ScoreRepeat = iota
	ScoreRemindLess
	ScoreRemindMore
	ScoreRemindAfter // accompanied by duration
)

/* broadcast messeage construction rule */
// any information about how particular message was constructed
type BCMCR interface{
	Score() int
}

type CRStruct struct{
	score int
}

func (crs CRStruct) Score() int {
	return crs.score
}

/* broadcast message model */
type BCMM interface{
	Next(cr BCMCR) string
//	Text() string
}

type BCMMreinforcement struct{
	items []Item
	weights map[string]int
	curr *Item
	prev *Item
}

func (m *BCMMreinforcement) Next(cr BCMCR) string {
	m.weights[m.curr.Id] += 3
	m.weights[m.prev.Id] += 2
	ballot := make([]int, 0)
	for idx, item := range m.items {
		w := m.weights[item.Id]
		if w < 1 { w = 1 }
		for i := 0; i < w; i++ { ballot = append(ballot, idx) }
	}
	rand.Shuffle(len(ballot),
		func(i, j int) {
				ballot[i], ballot[j] = ballot[j], ballot[i]
		})
	m.prev = m.curr
	currItem := m.items[ballot[0]]
	m.curr = &currItem
	return currItem.Id
}

type BCastMsg struct{
	text string
	consRule BCMCR
}

// if something connects, it's something to learn from
type BCastB struct{
	// of course, it has to be supplied with some input
	MsgC chan string
	NextC chan struct{}
}

type BCast interface{
	Bundle() BCastB
	Res() string
}

type reinforcementBCast struct{
	model BCMM
}

func (rbc *reinforcementBCast) Bundle() BCastB {
	b := BCastB{}

	return b
}

func (rbc *reinforcementBCast) Res() string {
	// todo: fulfill interface contract with concrete type
	var m BCMMreinforcement
	/*
	m, ok := rbc.model.(BCMMreinforcement)
	if !ok {
		return fmt.Sprintf("unknown type of model <%T>", rbc.model)
	}
	*/
	rv := ""
	for _, item := range m.items {
		rv += item.Id + "=" + strconv.Itoa(m.weights[item.Id]) + "\n"
	}
	return rv
}

type TcpConn struct {
	Port int
}

// this is probably the lowest-level interface
// to preserve when implementing Zsh client broadcasts:
// provide with some type (e.g. a function or type
// that provides messages based on, e.g., time of day)
// of input data and a connection type.
// these Zsh client broadcasts should log their
// activity, but there's no need to automate the
// ML feedback too much since a Zsh client means
// easy, also timestamp loggable, input to the computer.
//
// .Bundle() is a passthrough on reinforcementBCast as is.

// todo: listen for incoming tcp connections
//     on port from environment variable
//   and handle each connection by calling this function
//   with a parameter from the standard library
//   representing the tcp connection.
func NewReinforcementBCast(items []Item,
	tcpConn TcpConn,
) reinforcementBCast {
	m := BCMMreinforcement{}
	m.items = items
	m.weights = make(map[string]int)
	nC := make(chan struct{})
	mC := make(chan string)
	rbc := reinforcementBCast{
		model: &m,
	}
	go func() {
		defer close(mC)
	for {
		var nextOk, timeoutOk bool
		tC := make(chan struct{})
		go func() { time.Sleep(10 * time.Second); close(tC) }()
		select {
		case _, nextOk = <-nC:
		case <-tC:
			timeoutOk = true
		}
		if !nextOk {
			return
		}
		var cr BCMCR
		if timeoutOk {
			cr = CRStruct{ score: ScoreRemindMore }
		} else {
			cr = CRStruct{ score: ScoreRemindLess }
		}
		mC <- m.Next(cr)
	}
	}()
	return rbc
}

func democlirepl() {
	lines := make(chan string)
	go readLines(lines)
	for line := range lines {
		var ststmnt CliStstmntLn_00
		ststmnt = CliStstmntLn_00(line)
		if err := handleStatement(ststmnt); err != nil {
			fmt.Println("error handling statement %q", ststmnt)
			fmt.Println(err)
		}
	}
}

func demoliludallasmulticat() {
	var err error

	ftpConn, err = NewFtpConn()
	if err != nil {
		fmt.Println("error creating ftp connection")
		fmt.Println(err)
	}

	var bcasts []BCast
	broadCastsOpened := make(chan struct{})
go func () {
	/* deal with incoming tcp connections using stdlib
	 * here or in NewReinforcementBCast()
	 */
	defer close(broadCastsOpened)
	time.Sleep(1 * time.Minute)
	var bc BCast
	WithYamlDB(func(yamlDb *YamlDB) (err error) {
		rbc := NewReinforcementBCast(yamlDb.Items, TcpConn{Port: 6001})
		bc = &rbc
		return nil
	})
	bcasts = append(bcasts, bc)
}()


	lines := make(chan string)
	go readLines(lines)
	for line := range lines {
		var ststmnt CliStstmntLn_00
		ststmnt = CliStstmntLn_00(line)
		if err := handleStatement(ststmnt); err != nil {
			fmt.Println("error handling statement %q", ststmnt)
			fmt.Println(err)
		}
	}

	for _ = range broadCastsOpened {}
for _, bcast := range bcasts {
	close(bcast.Bundle().NextC)
	for _ = range bcast.Bundle().MsgC {}
	// todo: serialize to db
	fmt.Println("broadcast res")
	fmt.Println(bcast.Res())
}

}

func main() {
//	demoliludallasmulticat()
	demogcal()
}

func firstFileThatExists(tokFiles []string) string {
	for _, mTokFile := range tokFiles {
    _, statErr := os.Stat(mTokFile)
    switch {
    case os.IsNotExist(statErr):
    case statErr != nil:
	// could percolate errors..
//        return nil, err
    default:
		return mTokFile
	}
	}
	return ""
}


func getGClient() (*http.Client, error) {
	currUser, err := user.Current()
	if err != nil { return nil, err }
	ctx := context.Background()
	credFile := firstFileThatExists([]string{
		"credentials.json",
		filepath.Join(currUser.HomeDir, "credentials-print-gcal.json"),
	})
// https://developers.google.com/calendar/quickstart/go
b, err := ioutil.ReadFile(credFile)
if err != nil {
	fmt.Errorf("Unable to read client secret file: %v", err)
}
        // If modifying these scopes, delete your previously saved token.json.
        config, err := gauth.ConfigFromJSON(b, cal.CalendarReadonlyScope)
        if err != nil {
                return nil, fmt.Errorf("Unable to parse client secret file to config: %v", err)
        }

	var tok *oauth.Token
	tokFile := firstFileThatExists([]string{
		"token.json",
		filepath.Join(currUser.HomeDir, "token-print-gcal.json"),
	})

if tokFile != "" {
        f, err := os.Open(tokFile)
        if err != nil {
                return nil, err
        }
        defer f.Close()
        tok = new(oauth.Token)
        err = json.NewDecoder(f).Decode(tok)
	if err != nil { return nil, err }
} else {

        authURL := config.AuthCodeURL("state-token", oauth.AccessTypeOffline)
	fmt.Printf("Go to the following link in your browser then type the "+
	"authorization code: \n%v\n", authURL)

	var authCode string
	_, err = fmt.Scan(&authCode)
	if err != nil {
		return nil, fmt.Errorf("Unable to read authorization code: %v", err)
	}

        tok, err = config.Exchange(ctx, authCode)
	if err != nil {
		return nil, fmt.Errorf("Unable to retrieve token from web: %v", err)
	}
        fmt.Printf("Saving credential file to: %s\n", tokFile)
	f, err := os.OpenFile(tokFile, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0577)
        if err != nil {
			return nil, fmt.Errorf("Unable to cache oauth token: %v", err)
        }
        defer f.Close()
        json.NewEncoder(f).Encode(tok)
}

        return config.Client(context.Background(), tok), nil
}

func getGCal() (*cal.Service, error) {
	client, err := getGClient()
	if err != nil { return nil, err }
	// ...
	calendarService, err := cal.New(client)
	return calendarService, nil
}

func withGCal(fn func(svc *cal.Service)) error {
	svc, err := getGCal()
	if err != nil { return err }
	fn(svc)
	return nil
}

// todo: .ics layer for compatibility
func gcalGetEvents(svc *cal.Service) ([]*cal.Event, error) {
	t := time.Now().Format(time.RFC3339)
	events, err := svc.Events.List("primary").ShowDeleted(false).SingleEvents(true).TimeMin(t).Do()
	if err != nil { return nil, err }
	return events.Items, nil
}

// caller should nil-test out.GCalId ?= ""
// in case there was no error but event cannot be coerced to item..
func gcalEventToYamlDbItem(ev *cal.Event) (out Item, err error) {
	tags := []Tag{"gcal"}

	// there could be additional filters...
	if ev.Start.DateTime == "" ||
		ev.End.DateTime == "" {
		return Item{}, nil
	}


	out.Start, err = time.Parse(time.RFC3339, ev.Start.DateTime)
	if err != nil { return }

	out.End, err = time.Parse(time.RFC3339, ev.End.DateTime)
	if err != nil { return }

	out.Tags = tags
	out.GCalId = ev.ICalUID
	out.Name = ev.Summary
	// template out.Description from additional ev fields
	//  https://godoc.org/google.golang.org/api/calendar/v3#Event
	return
}

// use at least to add items from gcal fetch.
// currently only semi-generalized and checked for consistency
// with assumptions elsewhere in the program.
//
func createOrUpdateItems(newItems []Item, items []Item) ([]Item, error) {
	rv := make([]Item, 1)
	newGCalIds := make(map[string]bool)
	newIds := make(map[string]bool)
	for _, item := range newItems {
		if item.Id != "" && item.GCalId != "" ||
			item.Id == "" && item.GCalId == "" {
			return rv, errors.New("items to add expected to have " +
				"precisely one, uuid or icaluid only")
		}
		if item.Id != "" { newIds[item.Id] = true }
		if item.GCalId != "" { newGCalIds[item.GCalId] = true }
	}
	// todo: consult wiki for memory-optimized filter implementation
	for _, item := range items {
		if newGCalIds[item.GCalId] || newIds[item.Id] { continue }
		rv = append(rv, item)
	}
	for _, item := range newItems { rv = append(rv, item) }
	return rv, nil
}

// "sync" is actually unidirectional import.
// todo: ensure the rest of the program doesn't modify gcal events.
// the intent for now is that data from gcal is imported to immutable storage.
// only further syncs can mutate data from gcal.
func ModYamlDbGCalSync() ModYamlDBFn {
return func(yamlDb *YamlDB) error {
	svc, err := getGCal()
	if err != nil { return err }
	evs, err := gcalGetEvents(svc)
	if err != nil { return err }
	newItems := make([]Item, 0)
	for _, ev := range evs {
		item, err := gcalEventToYamlDbItem(ev)
		if err != nil { return err }
		if item.GCalId == "" { continue }
		newItems = append(newItems, item)
	}
	updatedItems, err := createOrUpdateItems(newItems, yamlDb.Items)
	if err != nil { return err }
	yamlDb.Items = updatedItems
	return nil
}
}

// read lines from standard input
//
// todo:  graceful exit on EOF
func readLines(lines chan<- string) {
	defer func() { close(lines) }()
	scanner := bufio.NewScanner(os.Stdin)
	var buf []byte
	for {
		fmt.Print("> ")
		if !scanner.Scan() {
			if scanner.Err() != nil {
				fmt.Println("input error: ", scanner.Err())
			}
		}
		buf = append(buf, scanner.Bytes()...)
		input := strings.TrimSpace(string(buf))
		lines <- input
	}
}

///// demos (i.e. main() replacements)

func demogcal() error {
	svc, err := getGCal()
	if err != nil { return err }
	evs, err := gcalGetEvents(svc)
	if err != nil { return err }
	items := make([]Item, 0)
	for _, ev := range evs {
		var item Item
		item, err = gcalEventToYamlDbItem(ev)
		if err != nil { return err }
		if item.GCalId == "" { continue }
//		fmt.Printf("event:\n%v\n----\nitem:\n%v\n", ev, item)
//		if evIdx != len(evs) - 1 { fmt.Println("====") }
		items = append(items, item)
	}
	const ts = `{{.Name}},{{.Start}},{{.End}}`
	t := template.Must(template.New("itemViewTemplate").Parse(ts))
	out := os.Stdout
	for _, item := range items {
//		fmt.Println(item)
		if e := t.Execute(out, item); e != nil { panic(e) }
		fmt.Println("")
	}
	return nil
}

func democreateread() {
	if err := WithYamlDB(ModYamlDbAddItem(
		Item{ Name: "an item at " + time.Now().String() },
	)); err == nil {
		fmt.Println("error adding item")
		fmt.Println(err)
	}

	if err := WithYamlDB(modYamlDbMultLinePlainCerial); err == nil {
		fmt.Println("error serializing database")
		fmt.Println(err)
	}
}

func demomarshall() {
	mi := Item{
		Name: "an item",
	//	Id: 10,
	}

	fmt.Println("initialized item")
	sb, err := yaml.Marshal(mi)
	if err != nil {
		fmt.Println("error")
		return
	}
	fmt.Println(string(sb))
}
