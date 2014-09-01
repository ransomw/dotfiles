#! /bin/sh

BKUP_TIME=`date +%y%m%d-%H%M`
BKUP_MEDIA=/media/blk
BKUP_DIR=$BKUP_MEDIA/bkup

# mount backup media
if ! cat /etc/mtab | cut -d" " -f 2 | grep -q $BKUP_MEDIA
then
		if ! mount $BKUP_MEDIA; then
				echo "couldn't mount $BKUP_MEDIA" 1>&2
				exit 1
		fi
fi

anki_bkup ()
{
		cp -r /home/ransom/Anki $BKUP_DIR/anki/$BKUP_TIME
}
notepad_bkup ()
{
		cp /home/ransom/documents/notepad.txt $BKUP_DIR/notepad/$BKUP_TIME.txt
}
config_bkup ()
{
		cp -r /home/ransom/config/ \
				$BKUP_DIR/config/$BKUP_TIME
}


anki_bkup
notepad_bkup
config_bkup

echo "total disk usage after backup"
df | grep "$BKUP_MEDIA$" | tr -s ' ' | cut -d" " -f 5

umount $BKUP_MEDIA
