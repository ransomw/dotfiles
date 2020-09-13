#! /usr/bin/env zsh

#
# backup select entries from freebsd fs
# to a thumb drive (or other mounted media).
#
# exit non-zero if backup fails.
# exit codes 4 and 5 are viewed like 4xx and 5xx
# http errors, user and system errors, resp.
#
# backups are stored by appending the
# [absolute] path in *.lines (read script)
# to BACKUP_DIR_RSYNC.  then the tarballs
# comprising the backup again contain
# the absolute path <- observed behavior.
#

setopt ERR_EXIT

EXEC_PATH=$0
if [[ -L $EXEC_PATH ]]; then
    SCRIPT_PATH=$(readlink $EXEC_PATH)
else
    SCRIPT_PATH=$EXEC_PATH
fi

SCRIPT_DIR="$( cd "$( dirname "$SCRIPT_PATH" )" && pwd )"

BKUP_MEDIA=/media/bthumb

BKUP_DIR_RSYNC=$BKUP_MEDIA/bkup/rsync_tar

STAGING_AREA=/tmp/bsdbkup_stage/
# todo: log rotation.  tooling?
TAR_LOG=/tmp/bsdbkup_stage/compression.log

if [[ -d $STAGING_AREA ]]; then
    rm -r $STAGING_AREA
fi
mkdir $STAGING_AREA

rsync_tar_bkup_file_or_dir ()
{
    file_or_dir_path=$1
    print -- "backing up ${file_or_dir_path}"
    if [[ $file_or_dir_path[1] != '/' ]]; then
        print -- 'backup paths must be absolute' 1>&2
        exit 4
    fi
    if [[ ! -e $file_or_dir_path ]]; then
        1>&2 print -- "Err: NOENT ${file_or_dir_path}"
        exit 1
    fi
    dir_path=$(dirname $file_or_dir_path)
    if [[ ! '.' = $dir_path ]]; then
        mkdir -p ${STAGING_AREA}/${dir_path}
        mkdir -p ${BKUP_DIR_RSYNC}/${dir_path}
    fi
    path_archive=${STAGING_AREA}/${file_or_dir_path}.tar
    print -- "compressing ${path_archive}" |tee -a $TAR_LOG
    # -c --create
    tar -cv \
        -f ${path_archive} \
        ${file_or_dir_path} &> $TAR_LOG
    print -- "rsyncing to ${BKUP_DIR_RSYNC}/${file_or_dir_path}.tar"
    # ??? -a flag to rsync
    rsync -cPr \
          ${path_archive} \
          ${BKUP_DIR_RSYNC}/${file_or_dir_path}.tar
    rm ${path_archive}
}

rsync_tar_bkup ()
{
    #
    # file_list_path=${SCRIPT_DIR}/rsync_files.list
    # dir_list_path=${SCRIPT_DIR}/rsync_dirs.list
    #
    file_list_path=${HOME}/.config/bkup/rsync_files.list
    dir_list_path=${HOME}/.config/bkup/rsync_dirs.list
    if [[ ! -r $file_list_path ]]; then
        print -- "backup file list $file_list_path not readable" 1>&2
        exit 5
    fi
    if [[ ! -r $dir_list_path ]]; then
        print -- "backup directory list $dir_list_path not readable" 1>&2
        exit 5
    fi
    file_list_str=$(cat $file_list_path |sed '/^[[:space:]]*$/d')
    dir_list_str=$(cat $dir_list_path |sed '/^[[:space:]]*$/d')
    file_list=(${(f)file_list_str})
    dir_list=(${(f)dir_list_str})
    for path_bkup in $file_list; do
        if [[ ! -e $path_bkup ]]; then
            print -- "$path_bkup in file list doesn't exist" 1>&2
            exit 2
        fi
        if [[ $path_bkup[1] != '/' ]]; then
            print -- "$path_bkup is not absolute it begins with" $path_bkup[1]
            print -- 'backup paths must be absolute' 1>&2
            exit 4
        fi
    done
    for path_bkup in $dir_list; do
        if [[ ! -e $path_bkup ]]; then
            print -- "$path_bkup in directory list doesn't exist" 1>&2
            exit 2
        fi
        if [[ $path_bkup[1] != '/' ]]; then
            print -- "$path_bkup is not absolute it begins with" $path_bkup[1]
            print -- 'backup paths must be absolute' 1>&2
            exit 4
        fi
    done
    # could backup all files to same tar archive in future
    print -- "backing up " $#file_list " files and " \
          $#dir_list " directories"
    for path_bkup in $file_list; do
        rsync_tar_bkup_file_or_dir $path_bkup
    done
    for path_bkup in $dir_list; do
        rsync_tar_bkup_file_or_dir $path_bkup
    done
}

# if [[ ! -d $BKUP_DIR_RSYNC ]]; then
#     echo "can't find backup media" 2>&1
#     exit 2
# fi

ensure_backup_media ()
{
    if [[ ! -d $BKUP_MEDIA ]]; then
        print -- "${BKUP_MEDIA} directory doesn't exist" 1>&2
        exit 5
    fi
    mount_points_str=$(mount | sed 's/.* on \(.*\) (.*)/\1/')
    mount_points=(${(f)mount_points_str})
    if (($mount_points[(Ie)$BKUP_MEDIA])); then
        print -- "${BKUP_MEDIA} is mounted"
    else
        print -- 'mount points are:'
        print -l -- $mount_points
        print -- "${BKUP_MEDIA} is not mounted" 1>&2
        exit 5
    fi
    if [[ ! -d $BKUP_DIR_RSYNC ]]; then
        print -- "backup directory $BKUP_DIR_RSYNC doesn't exist"
        if read -q yes_no'? create backup directory[y/N]'; then
            mkdir -p $BKUP_DIR_RSYNC
            print -- 'created backup directory'
        else
            print -- 'cannot backup without backup directory' 1>&2
            exit 4
        fi
    fi
}

ensure_backup_media

rsync_tar_bkup

echo "total disk usage after backup"
df | grep "$BKUP_MEDIA$" | tr -s ' ' | cut -d" " -f 5
