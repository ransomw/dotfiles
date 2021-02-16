
print_array()
{
    local arr_name=$1
    local cnt=0
    # the (P) is "parameter name expansion"
    # http://zsh.sourceforge.net/Doc/Release/Expansion.html#Parameter-Expansion-Flags
    for el in ${(P)${arr_name}}; do
        ((cnt++))
        print -- $cnt ":" $el
    done
}

build_array()
{
    local arr_name=$1
    if [[ -z $arr_name ]]; then
        1>&2 print -- 'provide a name for your array'
        return 1
    fi
    # -g is for global
    typeset -g -a $arr_name
    # because $arr_name=() isn't valid, use `set`
    # http://zsh.sourceforge.net/Doc/Release/Parameters.html#Array-Parameters
    set -A $arr_name
    print -- 'enter values one line at a time'
    print -- 'EOF (Ctrl-D) to stop array'
    while read el; do
        # append element to end of the array
        set -A $arr_name ${(P)${arr_name}} $el
    done
    print -- "created array '${arr_name}' with ${(P)#${arr_name}} elements"
}

quote_lines()
{
    sed "s/^\(.*\)$/'\1'/"
}


###


change_urxvt_font() {
    typeset -A fonts
    fonts=(
        'small' 'xft:Courier New:size=12'
        'medium' 'xft:Courier New:size=15'
        'large' 'xft:Courier New:size=18'
    )
    printf '\e]710;%s\007' $fonts[$1]
}

inc_urxvt_font_size() {
    font_str=$(appres urxvt \
                   |grep 'urxvt.font' \
                   |sed 's/^[^:]*://' \
                   |sed 's/[[:blank:]]//g')
    font_size=$(
        print -- font_str \
            |sed 's/.*size=\([[:digit:]]*\).*/\1/')
    #??? test for number?
}

###

# py_alarm_call

####
# FreeBSD system audio functionality <- to dotfiles
#

__audio_avg_curr_vol()
{
    local lr_vol=$(mixer -f /dev/mixer0 vol \
                 |tr -s ' ' | cut -d ' ' -f 7)
    local l_vol=$(print -- $lr_vol |cut -d ':' -f 1)
    local r_vol=$(print -- $lr_vol |cut -d ':' -f 2)
    local avg_vol
    ((avg_vol = (l_vol + r_vol) / 2))
    print -- $avg_vol
}

__audio_pan_effect()
{
    local delay_sec=7
    local vol_lo=35
    local vol_hi=65
    seq 1 100 | while read idx; do
        mixer -f /dev/mixer0 vol $vol_hi:$vol_lo
        sleep $delay_sec
        mixer -f /dev/mixer0 vol \
              $vol_lo:$vol_hi
        sleep $delay_sec
    done
    local vol_final=$(print -- \
                            "(${vol_lo} + ${vol_hi}) / 2" \
                          |bc)
    mixer -f /dev/mixer0 vol $vol_final:$vol_final
}

# todo: start from current volume,
#    parameterize with time and +/- difference
__audio_slow_volume()
{
    # local duration_min=$1
    # # todo: verify args, type vars
    # ((delay_sec=duration_min * 60 / 100))
    local delay_sec=1.25
    local vol_start=40
    local vol_end=60
    local vol_curr
    seq 0 100 | while read perc; do
        print -- "at ${perc} percent"
        ((vol_curr = vol_start +
          (vol_end - vol_start) * perc / 100  ))
        mixer -f /dev/mixer0 vol $vol_curr:$vol_curr
        sleep $delay_sec
    done
}


####
# video editing -- and playback?

clip_video()
{
    input=$1
    output=$2
    start=00:00:00
    end=00:00:17
    if [[ ! -z $3 ]]; then
        start=$3
    fi
    if [[ ! -z $4 ]]; then
        end=$4
    fi

    print -- "
input=$input
output=$output
start=$start
end=$end
"

    ffmpeg \
        -i $input \
        -ss $start -to $end \
        -c:v copy -c:a copy \
        $output

}

# todo: add chapters, merge videos

######
# espeak and xmessaging

mantra()
{
    if [[ $2 = "" ]]; then
        while true; do
            print -- $1 | \
                espeak -a 25 -s 75
        done
    else
        seq 1 $2 | while read idx; do
            print -- $1 | \
                espeak -a 25 -s 75
        done
    fi
}

## todo:  use a TRAP to dismiss an
#   indefinite (i.e. `while true`)
#   command (er, um, espeak) loop

__espeak_seq__behav()
{
    local tot=18
    local vol=175
    local start=$(date)
    seq 1 $tot | while read idx; do
        print -- "$idx of $tot" \
            |espeak -p 10 -s 20 -v m1 -a 190
        print 'cmd. cmd.' \
            |espeak -s 80 -a $vol -v f4 -p 110
        sleep 3
        print 'cmd. cmd.  motivate.' \
            |espeak -s 110 -a $vol -v f4 -p 110
        sleep 23
        print 'cmdb.' \
            |espeak -s 20 -a $vol -v f4 -p 110
        print 'constructive_criticism(cmdb), cmdb.' \
            |espeak -s 20 -a $vol -v m1 -p 10
        sleep 5
        print 'cmdb. cmdb. cmdb.' \
            |espeak -s 20 -a $vol -v f4 -p 110
        sleep 34
    done
    end=$(date)
    print -- "ran from $start to $end"
}

