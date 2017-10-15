#! /bin/sh

FONT_SIZE=24

while getopts "s:" opt; do
    case "$opt" in
        s) FONT_SIZE=$OPTARG
           ;;
    esac
done

# getopts cleanup
shift $((OPTIND-1))
[ "$1" = "--" ] && shift

INPUT=$1
TEXT=$2
if [ $# = 3 ]; then
    OUTPUT=$3
else
    OUTPUT="$(dirname $INPUT)/text_added__$(basename $INPUT)"
fi

HEIGHT=$(echo "2 * $FONT_SIZE" | bc)

ffmpeg -i $INPUT -vf \
       "format=yuv444p, \
        drawbox=y=ih/PHI:\
        color=black@0.4:\
        width=iw:\
        height=$HEIGHT:\
        t=max, \
        drawtext=fontfile=OpenSans-Regular.ttf:\
        text='${TEXT}':\
        fontcolor=white:\
        fontsize=${FONT_SIZE}:\
        x=(w-tw)/2:\
        y=(h/PHI)+th, \
        format=yuv420p" \
       -c:v libx264 -c:a copy -movflags +faststart \
       "$OUTPUT"
