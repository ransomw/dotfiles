#! /bin/sh

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"

BKUP_TIME=`date +%y%m%d-%H%M`
BKUP_MEDIA=/media/passport

BKUP_DIR_UNISON=$BKUP_MEDIA/files/bkup/unison
BKUP_DIR_GIT_REPOS="$BKUP_MEDIA/files/bkup/stardate$BKUP_TIME"

# mount backup media
if ! cat /etc/mtab | cut -d" " -f 2 | grep -q $BKUP_MEDIA
then
		if ! mount $BKUP_MEDIA; then
				echo "couldn't mount $BKUP_MEDIA" 1>&2
				exit 1
		fi
fi

unison_local_bkup ()
{
    for path_bkup in $(cat "$SCRIPT_DIR/paths_unison_dirs")
    do
        unison $HOME $BKUP_DIR_UNISON -path $path_bkup
    done
}

git_local_bkup ()
{
    if [ ! -d $BKUP_DIR_GIT_REPOS ]
    then
        mkdir $BKUP_DIR_GIT_REPOS
    fi

    for path_src in $(cat "$SCRIPT_DIR/paths_git_repos")
    do
        echo "backing up $path_src ..."
        path_dest="$BKUP_DIR_GIT_REPOS/$(basename $path_src)"
        git clone $path_src $path_dest
        (
            cd $path_dest
            for branch in $(git branch -a | \
                                grep remotes/origin | \
                                sed 's/\ \ remotes\/origin\///' | \
                                grep -v "^HEAD")
            do
                git checkout $branch
            done
        )
        if [ -f "$path_src/TODO.org" ]; then
            cp "$path_src/TODO.org" $path_dest
        fi
    done
}

unison_local_bkup
git_local_bkup

echo "total disk usage after backup"
df | grep "$BKUP_MEDIA$" | tr -s ' ' | cut -d" " -f 5

exit 0

umount $BKUP_MEDIA
