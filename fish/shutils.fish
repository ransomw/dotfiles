function process_files
    argparse 'i/input=' 'o/output=?' -- $argv
    or return # Exit if argparse fails (e.g., missing required input)

    echo $argv

    if not set -q _flag_input

    end

    set input_file $_flag_input

end

function add_text
    set FONT_SIZE 24

    argparse 's/font-size=?' -- $argv
    or return # Exit if argparse fails (e.g., missing required input)

    if set -q _flag_font_size
        set FONT_SIZE _flag_font_size
    end

    set INPUT $argv[1]
    set TEXT $argv[2]
    if test (count $argv) -eq 3
        set OUTPUT $3
    else
        set OUTPUT "$(dirname $INPUT)/text_added__$(basename $INPUT)"
    end

    set HEIGHT $(echo "2 * $FONT_SIZE" | bc)

    ffmpeg -i $INPUT -vf \
        "format=yuv444p, \
        drawbox=y=ih/PHI:\
        color=black@0.4:\
        width=iw:\
        height=$HEIGHT:\
        t=max, \
        drawtext=fontfile=OpenSans-Regular.ttf:\
        text='$TEXT':\
        fontcolor=white:\
        fontsize=$FONT_SIZE:\
        x=(w-tw)/2:\
        y=(h/PHI)+th, \
        format=yuv420p" \
        -c:v libx264 -c:a copy -movflags +faststart \
        "$OUTPUT"
end

function bkup
    set PATHS_DIR "~/.config/bkup"

    set BKUP_TIME `date +%y%m%d-%H%M`
    set BKUP_MEDIA /media/passport

    set BKUP_DIR_UNISON $BKUP_MEDIA/files/bkup/unison
    set BKUP_DIR_GIT_REPOS "$BKUP_MEDIA/files/bkup/stardate$BKUP_TIME"

    # # mount backup media
    # if ! cat /etc/mtab | cut -d" " -f 2 | grep -q $BKUP_MEDIA
    # then
    # 		if ! mount $BKUP_MEDIA; then
    # 				echo "couldn't mount $BKUP_MEDIA" 1>&2
    # 				exit 1
    # 		fi
    # fi

    for path_bkup in (cat "$SCRIPT_DIR/paths_unison_dirs")
        unison $HOME $BKUP_DIR_UNISON -path $path_bkup
    end

    if not test -d $BKUP_DIR_GIT_REPOS
        mkdir $BKUP_DIR_GIT_REPOS
    end

    for path_src in (cat "$PATHS_DIR/paths_git_repos")
        echo "backing up $path_src ..."
        set path_dest "$BKUP_DIR_GIT_REPOS/$(basename $path_src)"
        git clone $path_src $path_dest
        set ret_dir (pwd)
        cd $path_dest
        for branch in (git branch -a | \
            grep remotes/origin | \
            sed 's/\ \ remotes\/origin\///' | \
            grep -v "^HEAD")
        git checkout $branch
        end
        cd $ret_dir
        if test -f "$path_src/TODO.org"
            cp "$path_src/TODO.org" $path_dest
        end
    end


    echo "total disk usage after backup"
    df | grep "$BKUP_MEDIA" | tr -s ' ' | cut -d" " -f 5

    # return
    # umount $BKUP_MEDIA
end



##todo: dir_diff

function get_fastmail_cert
    set CERT_FILE ~/.fastmail.crt
    echo "saving gmail certificate to $CERT_FILE"

    set CERT_RES $(openssl s_client -connect imap.fastmail.com:993 \
        -showcerts \
        2>&1  < /dev/null \
        |sed \
        '/^-----BEGIN CERTIFICATE-----$/,/^-----END CERTIFICATE-----$/!d')
    set CERT_STATUS $status

    if test $CERT_STATUS -eq 0
        echo $CERT_RES > $CERT_FILE
        echo 'saved certificate'
    else
        echo "failed to get gmail cert $CERT_RES ($CERT_STATUS)" 2>&1
    end

end


function get_gmail_cert
    set CERT_FILE ~/.gmail.crt
    set CERT_RES $(openssl s_client -connect imap.gmail.com:993 \
        -showcerts \
        2>&1  < /dev/null \
        | sed -ne '/-BEGIN CERTIFICATE-/,/-END CERTIFICATE-/p' \
        | head -n 27)

    if test $status -eq 0
        echo "saving gmail certificate" \
            "\n$CERT_RES\n" \
            "to $CERT_FILE"
        echo "$CERT_RES" > $CERT_FILE
    else
        echo "failed to get gmail cert $CERT_RES" 2>&1
    end
end

function gifmaker
    # convert video to animated gif
    #
    # example usage
    # ./gifmaker.sh -p 50 -s 1:32:55 -e 17 -v ~/video/strange.mp4 ~/tmp/strange.gif
    # # starting 1 hour, 32 min, 55 seconds in;
    # # scaled to 50%, verbose mode;
    # # seventeen seconds.
    set SCALE 100
    set VERBOSE 0
    set CLEAN_FRAMES 1
    set FRAMES_PER_SECOND 5
    set CENTI_SEC_PER_FRAME (echo "100 / $FRAMES_PER_SECOND" | bc)
    set FRAMES_DIR/tmp/frames
    set START_SEEK_OPT
    set END_SEEK_OPT

    argparse 'v/verbose=&' 'c/clean-frames=&' 's/start-seek=' 'e/end-seek' 'p/scale='  -- $argv

    set START_SEEK_OPT "-ss $_flag_start_seek"
    set END_SEEK_OPT "-t $_flag_end_seek"

    if set -q _flag_clean_frames
        set CLEAN_FRAMES 0
    end

    set SCALE $_flag_scale
    if set -q _flag_verbose
        set VERBOSE=1
    end

    set SCALE_OPT "-scale $SCALE"'%'

    if not test -d "$FRAMES_DIR"
        mkdir "$FRAMES_DIR"
    end

    set CLIP_FILE "$FRAMES_DIR"'/clip__'$(basename $1)

    if test $VERBOSE -eq 1
        echo "clipping video"
        set -x
    end

    ffmpeg \
        $START_SEEK_OPT $END_SEEK_OPT \
        -i $1 \
        -acodec copy -vcodec copy \
        $CLIP_FILE
    set +x

    if test $VERBOSE -eq 1
        echo "extracting frames to jpg"
        set -x
    end

    ffmpeg \
        -i $CLIP_FILE \
        -r $FRAMES_PER_SECOND \
        "$FRAMES_DIR"'/frame-%03d.jpg'
    set +x

    if test $VERBOSE -eq 1
        echo "converting frames to gif"
        set -x
    end

    for frame_jpg in (ls "$FRAMES_DIR/"*.jpg)
        set frame_gif (echo $frame_jpg | sed 's/jpg$/gif/')
        convert \
            $SCALE_OPT \
            $frame_jpg $frame_gif
    end

    set +x

    if test $VERBOSE -eq 1
        echo "creating animated gif"
        set -x
    end

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

    if test $CLEAN_FRAMES -eq 1
        if test $VERBOSE -eq 1
            echo "cleaning up artifacts"
            set -x
        end
        rm "$FRAMES_DIR/"*
        set +x
    else
        if test $VERBOSE -eq 1
            echo "leaving artifacts in $FRAMES_DIR"
        end
    end
end

function latex_cts_build
    set NAME $argv[1]

    # if [ -z ${PDFVIEWER+x} ]; then
    set PDFVIEWER zathura
    # fi

    if not test -f "$NAME.tex"
        echo "couldn't find book file $NAME.tex" 1>&2
        return
    end

    if test -d "$NAME"
        set find_paths "$NAME.tex $NAME/"
    else
        set find_paths "$NAME.tex"
    end

    while true
        find $find_paths -name '*.tex' |\
            entr -c -d -r -s "lualatex -halt-on-error $NAME.tex && $PDFVIEWER $NAME.pdf"
    end
end

function record
    arecord \
        -f cd \
        -d 3600 \
        -t wav \
        | lame --preset 56 -mm - `date +%Y%m%d%H%M`.mp3
end

function clip_video
    set input $argv[1]
    set output $argv[2]
    set start 00:00:00
    set end_time 00:00:17
    if test (count $argv) -gt 2
        set start $argv[3]
    end
    if test (count $argv) -gt 3
        set end_time $argv[4]
    end

    echo "
input=$input
output=$output
start=$start
end=$end_time
"

    ffmpeg \
        -i $input \
        -ss $start -to $end_time \
        -c:v copy -c:a copy \
        $output
end
