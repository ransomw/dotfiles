#! /bin/sh

arecord \
    -f cd \
    -d 3600 \
    -t wav \
    | lame --preset 56 -mm - `date +%Y%m%d%H%M`.mp3
