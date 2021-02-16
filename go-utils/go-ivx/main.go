package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"time"
)

/* golang X11 image viewer
 * w/ cmd read from stdin
 *
 * todo
 * ----
 * o background process from urxvt a no-go
 * o socket instead of stdio (both client and server)
 * o in-image text display (img info, terminal, etc.)
 */


func main() {
	iC := make(chan string)
	files := os.Args[1:]

go func() {
    lines := make(chan string)
    go readLines(lines)
    for line := range lines {
		iC <- line
	}
}()

if len(files) > 1 {
	go func() {
	var idx int
	for {
	time.Sleep(2 * time.Second)
//	iC <- files[idx]
	idx++
	idx %= len(files)
	}
	}()
}

	displayX11(iC, files, 300, 300)
}

// todo:  graceful exit on EOF
func readLines(lines chan<- string) {
    defer func() { close(lines) }()
    for {
		var buf []byte
		scanner := bufio.NewScanner(os.Stdin)
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

