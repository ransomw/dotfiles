package main

import (
	"context"
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"os"
	"os/user"
	"path/filepath"


//	"google.golang.org/api/googleapi/transport"
	oauth "golang.org/x/oauth2"
	gauth "golang.org/x/oauth2/google"
	"google.golang.org/api/youtube/v3"
)

var (
	query      = flag.String("query", "Google", "Search term")
	maxResults = flag.Int64("max-results", 25, "Max YouTube results")
)

func firstFileThatExists(tokFiles []string) string {
	for _, mTokFile := range tokFiles {
		_, statErr := os.Stat(mTokFile)
		switch {
		case os.IsNotExist(statErr):
		case statErr != nil:
		// could percolate errors..
		//		return nil, err
		default:
			return mTokFile
		}
	}
	return ""
}


func getGClient() (*http.Client, error) {

	currUser, err := user.Current()
	if err != nil{ return nil, err }

    credFile := firstFileThatExists([]string{
		"credentials.json",
		filepath.Join(currUser.HomeDir, "credentials-yt-search.json"),
	})


	ctx := context.Background()
// https://developers.google.com/calendar/quickstart/go
b, err := ioutil.ReadFile(credFile)
if err != nil {
	fmt.Errorf("Unable to read client secret file: %v", err)
}
	// If modifying these scopes, delete your previously saved token.json.
	config, err := gauth.ConfigFromJSON(b, youtube.YoutubeReadonlyScope)
	if err != nil {
		return nil, fmt.Errorf("Unable to parse client secret file to config: %v", err)
	}

	var tok *oauth.Token

    tokFile := firstFileThatExists([]string{
		"token.json",
		filepath.Join(currUser.HomeDir, "token-yt-search.json"),
	})


	_, statErr := os.Stat(tokFile)
	var tokFileExists bool
	switch {
	case os.IsNotExist(statErr):
	case statErr != nil:
		return nil, err
	default:
		tokFileExists = true
	}

if tokFileExists {
        f, err := os.Open(tokFile)
        if err != nil {
                return nil, err
        }
        defer f.Close()
        tok = new(oauth.Token)
        err = json.NewDecoder(f).Decode(tok)
	if err != nil { return nil, fmt.Errorf("couldn't deserialize config token %v", err) }
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
		log.Fatalf("Unable to retrieve token from web: %v", err)
	}
        fmt.Printf("Saving credential file to: %s\n", tokFile)
	f, err := os.OpenFile(tokFile, os.O_RDWR|os.O_CREATE|os.O_TRUNC, 0600)
        if err != nil {
			return nil, fmt.Errorf("Unable to cache oauth token: %v", err)
        }
        defer f.Close()
        json.NewEncoder(f).Encode(tok)
}

        return config.Client(context.Background(), tok), nil
}


func main() {
	flag.Parse()

	client, err := getGClient()
	if err != nil {
		log.Fatalf("Error creating new GHTTP client: %v", err)
	}


	service, err := youtube.New(client)
	if err != nil {
		log.Fatalf("Error creating new YouTube client: %v", err)
	}

	// Make the API call to YouTube.
	call := service.Search.List("id,snippet").
		Q(*query).
		MaxResults(*maxResults)
	response, err := call.Do()
	if err != nil {
		log.Fatalf("Error doing YouTube search: %v", err)
	}

	// Group video, channel, and playlist results in separate lists.
	videos := make(map[string]string)
	channels := make(map[string]string)
	playlists := make(map[string]string)

	// Iterate through each item and add it to the correct list.
	for _, item := range response.Items {
		switch item.Id.Kind {
		case "youtube#video":
			videos[item.Id.VideoId] = item.Snippet.Title
		case "youtube#channel":
			channels[item.Id.ChannelId] = item.Snippet.Title
		case "youtube#playlist":
			playlists[item.Id.PlaylistId] = item.Snippet.Title
		}
	}

	printIDs("Videos", videos)
	printIDs("Channels", channels)
	printIDs("Playlists", playlists)
}

// Print the ID and title of each result in a list as well as a name that
// identifies the list. For example, print the word section name "Videos"
// above a list of video search results, followed by the video ID and title
// of each matching video.
func printIDs(sectionName string, matches map[string]string) {
	fmt.Printf("%v:\n", sectionName)
	for id, title := range matches {
		fmt.Printf("[%v] %v\n", id, title)
	}
	fmt.Printf("\n\n")
}

