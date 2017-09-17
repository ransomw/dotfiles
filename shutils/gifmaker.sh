#! /bin/sh

# convert video to animated gif
#
# example usage
# ./gifmaker.sh -p 50 -s 1:32:55 -e 17 -v ~/video/strange.mp4 ~/tmp/strange.gif
# # starting 1 hour, 32 min, 55 seconds in;
# # scaled to 50%, verbose mode;
# # seventeen seconds.

SCALE=100
VERBOSE=0
CLEAN_FRAMES=1
FRAMES_PER_SECOND=5
CENTI_SEC_PER_FRAME=$(echo "100 / $FRAMES_PER_SECOND" | bc)
FRAMES_DIR=/tmp/frames

START_SEEK_OPT=
END_SEEK_OPT=

while getopts "vcs:e:p:" opt; do
    case "$opt" in
        s) START_SEEK_OPT="-ss $OPTARG"
           echo "$OPTARG"
           ;;
        e) END_SEEK_OPT="-t $OPTARG"
           ;;
        c) CLEAN_FRAMES=0
           ;;
        p) SCALE=$OPTARG
           ;;
        v) VERBOSE=1
           ;;
    esac
done

# getopts cleanup
shift $((OPTIND-1))
[ "$1" = "--" ] && shift

SCALE_OPT="-scale $SCALE"'%'

if [ ! -d "$FRAMES_DIR" ]; then
    mkdir "$FRAMES_DIR"
fi

CLIP_FILE="$FRAMES_DIR"'/clip__'$(basename $1)

if [ $VERBOSE = 1 ]; then
    echo "clipping video"
    set -x
fi

ffmpeg \
    $START_SEEK_OPT $END_SEEK_OPT \
    -i $1 \
    -acodec copy -vcodec copy \
    $CLIP_FILE
set +x

if [ $VERBOSE = 1 ]; then
    echo "extracting frames to jpg"
    set -x
fi

ffmpeg \
    -i $CLIP_FILE \
    -r $FRAMES_PER_SECOND \
    "$FRAMES_DIR"'/frame-%03d.jpg'
set +x

if [ $VERBOSE = 1 ]; then
    echo "converting frames to gif"
    set -x
fi

for frame_jpg in $(ls "$FRAMES_DIR/"*.jpg); do
    frame_gif=$(echo $frame_jpg | sed 's/jpg$/gif/')
    convert \
        $SCALE_OPT \
        $frame_jpg $frame_gif
done

set +x

if [ $VERBOSE = 1 ]; then
    echo "creating animated gif"
    set -x
fi

gifsicle \
    --delay $CENTI_SEC_PER_FRAME \
    --loop \
    "$FRAMES_DIR/"*.gif \
    > $2
set +x

## imagemagick giving Error "cache resources exhausted"
# convert \
#     -delay $CENTI_SEC_PER_FRAME \
#     -loop 0 \
#     "$FRAMES_DIR"'/*.gif' \
#     $2


if [ $CLEAN_FRAMES = 1 ]; then
    if [ $VERBOSE = 1 ]; then
        echo "cleaning up artifacts"
        set -x
    fi
    rm "$FRAMES_DIR/"*
    set +x
else
    if [ $VERBOSE = 1 ]; then
        echo "leaving artifacts in $FRAMES_DIR"
    fi
fi
