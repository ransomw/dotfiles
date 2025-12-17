
set PATH $HOME/bin $PATH

# https://9fans.github.io/plan9port/
set -gx PLAN9 $HOME/sbin/plan9
set PATH $PATH $PLAN9/bin

if test -f  ~/.pythonrc
    set -gx PYTHONSTARTUP $HOME/.pythonrc
end

if test -d ~/ws/go
    set -gx GOPATH $HOME/ws/go
end

alias godebug="dlv debug --headless --listen=:2345 --log --api-version=2 -- $argv"

alias l='less -R'
alias t='tee'
alias c='cat'
alias g='grep'

if which zathura &>/dev/null
    alias texdoc='PDFVIEWER=zathura texdoc'
end

# todo: window based on screen-size
alias grep_window='grep -A 3 -B 3'

# -h for human-readable file sizes
alias ls='ls -h'
# sort by descending creation date
alias ls_time='ls -ct'


# prevent man page line-wrap by fitting page
# to terminal
#
# $COLUMNS is a zsh "magic" variable
# the MANWIDTH env var is tested on FreeBSD..
# .. does this variable have an effect on all systems?
alias man='MANWIDTH=$COLUMNS man'


# uninstall packages that aren't specified in requirements.txt..
# .. recommended use:  `pip_prune && pip -r requirements.txt`
#        to install transitive deps.
alias pip_prune='pip freeze | grep -Fvx -f requirements.txt - | xargs pip uninstall -y'

# terminal based GG Hangouts client, available over `pip install hangouts`
alias hangups='hangups --col-scheme solarized-dark --key-prev-tab "ctrl p" --key-menu "ctrl o" --key-next-tab "ctrl n"'


# 3ditors
alias temacs='emacs -nw --no-desktop' # t is for terminal
# todo: xemacs to qtile mod4-r 'spawn' menu
alias xemacs='emacsclient -c || var=(emacs --daemon --no-desktop && sleep 2.3 && emacsclient -c) $var'
alias acme='acme -f $PLAN9/font/fixed/unicode.10x20.font'
