alias mpv='mpv --volume-max=250'
alias mpvvn='mpv -vid no --volume-max=250'
alias texdoc='PDFVIEWER=zathura texdoc'
# serve directory
alias sd='python -m http.server 3003'
alias l='less'
### todo: broken (mbsync segfault on "get" Group)
# alias m='mbsync get && mutt'
### ... hack "group" feat. w/ alias
alias m='mbsync gmail-fetch && mbsync gmail-fetch-sent && mbsync mutt-fetch && mbsync mutt-fetch-sent && mutt'
alias pyhtmldoc='elinks /usr/share/doc/python3/html/index.html'
alias pipprune='pip freeze | grep -Fvx -f requirements.txt - | xargs pip uninstall -y'
alias dashbd='watch -n 60 "acpi ; TZ=America\/Los_Angeles date"'
alias hangups='hangups --col-scheme solarized-dark --key-prev-tab "ctrl p" --key-menu "ctrl o" --key-next-tab "ctrl n"'
# startup https://9fans.github.io/plan9port/
alias p9p='PLAN9=~/workspace/upstream/plan9port export PLAN9; PATH=$PLAN9/bin:$PATH export PATH; rc'
