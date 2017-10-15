#! /bin/sh

NAME=$1

if [ -z ${PDFVIEWER+x} ]; then
    PDFVIEWER=zathura
fi

if [ ! -f "$NAME.tex" ]
then
    echo "couldn't find book file $NAME.tex" 1>&2
    exit 1
fi

if [ -d "$NAME" ]
then
    find_paths="$NAME.tex $NAME/"
else
    find_paths="$NAME.tex"
fi

while true
do
    find $find_paths -name '*.tex' |\
        entr -c -d -r -s "lualatex -halt-on-error $NAME.tex && \
                          $PDFVIEWER $NAME.pdf"
done
