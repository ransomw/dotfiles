#! /bin/sh

MAX_PRIORITY=99
DEBUG_FLAG=""

while getopts "dp:" opt; do
    case "$opt" in
        p) MAX_PRIORITY=$OPTARG
           ;;
        d) DEBUG_FLAG="--debug"
           ;;
    esac
done

# getopts cleanup
shift $((OPTIND-1))
[ "$1" = "--" ] && shift

THIS_SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
DP_SRC_SCRIPT_DIR="$THIS_SCRIPT_DIR/scripts"
DP_EXC_SCRIPT_DIR=/tmp/devilspie

if [ -d $DP_EXC_SCRIPT_DIR ]; then
    rm -r $DP_EXC_SCRIPT_DIR
fi

mkdir $DP_EXC_SCRIPT_DIR

for file in $(ls -1 $DP_SRC_SCRIPT_DIR | grep "\.lua$"); do
    file_priority=$(echo $file | sed 's/_.*//')
    if [ $(echo "$file_priority <= $MAX_PRIORITY" | bc) = 1 ]; then
        cp "$DP_SRC_SCRIPT_DIR/$file" $DP_EXC_SCRIPT_DIR
    fi
done

devilspie2 $DEBUG_FLAG \
           --folder $DP_EXC_SCRIPT_DIR
