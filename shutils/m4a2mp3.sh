#! /bin/sh

mkdir mp3
for path in "$@"
do
		ffmpeg -i "$path" -acodec libmp3lame -ab 128 -ac 2 mp3/"$path".mp3
done
#ffmpeg -i song.m4a -acodec libmp3lame -ab 128 -ac 2 song.mp3