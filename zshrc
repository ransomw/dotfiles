#-*-shell-script-*-

# run for interactive shells.  place in $ZDOTDIR or ~

### Guide opts and notes

# in zsh, a $calar variable doesn't need to be double-quoted:
# it's passed as a single argv string double-quoted or not,
# and arrays are used when it's desirable to splat shell variables
# accross a program's arguments.
# this is in contrast to bash and ksh, so the following opt can be
# set to change to more bash-like behavior in case this is confusing.
unsetopt SH_WORD_SPLIT # default

# here are a few misc. options that have to do with pattern-matching..
# BARE_GLOB_QUAL, GLOB_SUBST, SH_FILE_EXPANSION, SH_GLOB, KSH_GLOB
# NOMATCH, BAD_PATTERN
# .. the plan is to use zsh's "powerful" pattern-matching features
#   close to default, so these are unlikely to get changed

# with BG_NICE set, background jobs get scheduled lower than fg jobs
# todo: lookup relevant freebsd man pages on niceness and scheduling
setopt BG_NICE # default

# whether to notify of background jobs as soon as the finish
# or to notify after the next (possibly no-op) command is run.
setopt nonotify # non-default

# whether SIGHUP is sent to background jobs upon exiting the shell
setopt HUP # default

## line-editor config
# emacs- or vim-like editing is determined by $VISUAL and $EDITOR
# env vars by default or by the bindkey builtin if it's present

# this is the usual way to explicitly set emacs or vi mode
# bindkey -e # -e for emacs, -v for vi
# or, for emacs mode, since there are separate insert and command maps,
# bindkey -A emacs main
# aliases the main keymap to emacs and is equivalent to `bindkey -e`
#
# to keep the emacs keymap as-is, copy it into a new keymap and alias
# that to main
bindkey -N mymap emacs
bindkey -A mymap main

# keybindings
bindkey '\eF' vi-forward-word
bindkey '\eB' vi-backward-word
bindkey '\eK' kill-region
bindkey '\e-' redo
# forward and backward search command line to point
# ... '\ep' and '\en' move by first word on line.
bindkey '^xp' history-beginning-search-backward
bindkey '^xn' history-beginning-search-forward
# move from continuation lines to multi-line editing
# ... M-<ret> or ^v^j to directly insert newlines
bindkey '\eq' push-line-or-edit

# warning: `bindkey -m' disables multibyte support
# bindkey -m # ensure <ALT> acts as meta, just as <ESC> does

# bindkey '\eF' bash-forward-word
# bindkey '\eB' bash-backward-word


## history options
HISTFILE=~/.zhistory
# number of lines written to save file
SAVEHIST=1000
# number of lines saved in a shell instance
HISTSIZE=1000 # default: 30
# keep HISTSIZE >= SAVEHIST

# usually, the history file is overwritten whenever a shell exits
# the following opt appends each command to the file as it's executed
setopt INC_APPEND_HISTORY
# the history file is still read only when the shell starts up,
# so a shell's history isn't overwritten.
# setopt SHARE_HISTORY
# will cause the history file to be re-read into the shell after
# every command.

# this option stores some additional data (e.g. time) along with each
# command.  using it means the history file can't be read by other
# shells.  this data is accessible via the `history` command.
setopt EXTENDED_HISTORY
# here are the relevant flags to the history command
# -d -- start time
# -f -- start time and date
# -D -- elapsed time
# -i -E -- international and european date formatting

# the following prevents duplicate entries in the shell's history list,
# keeping newly added entries
setopt HIST_IGNORE_ALL_DUPS
# setopt HIST_IGNORE_DUPS
# is a less agressive version, removing only consecutive duplicates only,
# and the following does the same for the save file.
setopt HIST_SAVE_NO_DUPS
# and to keep the history particularly tidy,
setopt HIST_REDUCE_BLANKS
# normalizes \s before saving history entries.
# on beginning/end of history list in the line editor,
setopt NO_HIST_BEEP

# ... in fact, just turn off beeps altogether
setopt NO_BEEP

# don't be clever with `cd` and `pwd` in symlinked directories
setopt CHASE_LINKS

# allow exporting function-local variables with `typeset -x`
# then, when the function exits, unset the variable both
# in the shell and in the environment.
unsetopt GLOBAL_EXPORT

# zsh includes a script to load escape sequences for some colors
# into fg and bg arrays.  included colors are
# cyan, white, yellow, magenta, black, blue, red, grey, green
# ... the -U flag to `autoload` disables aliases when running the
#     loaded function and is recommended just about wherever
#     functions are autoloaded.
autoload -U colors && colors
# usage notes:
# colors is an two-directional associative array
# `typeset -Ag`, so ${(k)colors} and ${(v)colors} are the same.
#     print -l -- ${color} |grep -v '[0-9]'
# lists all available colors (and styles).
# meanwhile, access to the colors is via $fg and $bg arrays
# as well as $bold_color and $reset_color.
# example:
#     name=cyan ; text=hiya
#     print -- "$fg[$name]$text$reset_color"


# set prompt
# in addition to the `%>>` truncation operator and colors,
# this prompt uses the `%(?..` ternary operator to display
# red or green depending on the last command's exit code.
PS1='%10>...>'"%(?.%{${fg[green]}%}.%{${fg[red]}%})"'%1/'"%{$reset_color%}"'%>>%# '


# todo: install programs via system package manager
function install_program() {
    return 1
}


## user-defined path extensions
# `typeset -U` ensures uniqueness of elements in an array:
# duplicate elements after the first (from the left) are removed
typeset -U path
# and zsh auto-converts the $path array and $PATH env var
path=(~/bin $path)

# plan9port (Plan 9 from User Space)
# https://9fans.github.io/plan9port/
# cf. 9front.org (Plan 9 from Outer Space)
export PLAN9=/usr/home/ransom/sbin/plan9
path=($path "$PLAN9/bin")


### language environments

## python

# pyenv (https://github.com/pyenv/pyenv)
# provides interpreters of all versions
if [[ -d "$HOME/.pyenv" ]]; then
# not installing pyenv is fine
    export PYENV_ROOT="$HOME/.pyenv"
    # path updated by init (pretty sure?
    #path=("$PYENV_ROOT/bin" $path)
    if command -v pyenv 1>/dev/null 2>&1; then
        eval "$(pyenv init -)"
    fi
fi

# load this file on interpreter startup
if [[ -f ~/.pythonrc ]]; then
    export PYTHONSTARTUP="${HOME}/.pythonrc"
fi
# todo: prompt to add link otherwise


# caches upstream source, iirc..
# .. some confusion about module support.
if [[ -d "$HOME/ws/go" ]]; then
    export GOPATH="$HOME/ws/go"
fi

## VPSs


### aliases #

## shortcuts
alias l='less -R'
alias t='tee'
alias c='cat'
alias g='grep'

if whence zathura &>/dev/null; then
    alias texdoc='PDFVIEWER=zathura texdoc'
fi

# todo: only activate in venv -- shim?
alias pyfmt='black' # todo: add opts

## program wrappers

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
alias xemacs='emacsclient -c || (emacs --daemon --no-desktop && sleep 2.3 && emacsclient -c)'
alias acme='acme -f $PLAN9/font/fixed/unicode.10x20.font'

local -A _mpv_autofit_param_flags__ass_arr=(
    'autofit-smaller' '80%x80%'
    'autofit-larger' '80%x80%'
)

# todo: associative array (-A) to regular
#   array (-a) conversion, here && and in
#   cookbook.

local -a mpv_autofit_param_flags=(
    '--autofit-smaller=80%x80%'
    '--autofit-larger=80%x80%'
)



if [[ "$(uname)" = "FreeBSD" ]];then
    alias mpv='/usr/local/bin/mpv --audio-device=oss//dev/dsp0.0'" ${mpv_autofit_param_flags}"
else
    alias mpv='mpv'" ${mpv_autofit_param_flags}"
fi

alias mn='mpv --vid=no'
alias mpvm='mpv -mute="yes"'
alias mpvs='mpv --shuffle'


# 🪦🪦🪦
# unused: currently using phoneputer to access email.
#
#alias m='mbsync gmail-fetch && mbsync gmail-fetch-sent && mbsync mutt-fetch && mbsync mutt-fetch-sent && mutt'
#
# error: mbsync segfault on "get" Group (maybe??)
# alias m='mbsync get && mutt'
# ⛼


## imagined stuff ☺😊

serve_directory() {
    port=$1
    if [[ "$port" = '' ]]; then
        port=3003
    fi
    python -m http.server $port
}

alias ddg='links http://www.duckduckgo.com'

alias sd=serve_directory
alias serve_simple=sd
alias ssrv=sd

alias ppath='print -- $PATH |sed "s/:/\n/g"'
alias truncate='sed "s/^\(.\{$(($COLUMNS-3))\}\).*/\1$fg[cyan]$bold_color...$reset_color/"'
# a-la Python
alias reload='. ~/.zshrc'




if [[ "$(uname)" = "FreeBSD" ]]; then
    alias handbook='links /usr/local/share/doc/freebsd/handbook/index.html'

    function dashbd() {
        # typeset -g show_dashbd
        # typeset -l show_dashbd
        ##
        # ??? typeset doesn't apply to functions
        typeset -l function show_dashbd() {
            apm -l |tr -d "\n" && echo -n "% battery"
            # TZ env var to date is relpath to /usr/share/zoneinfo/ -- clocks
            echo 'clocks' &&\
                echo -n "    LA/SF/Bonanza    " && TZ=America\/Los_Angeles date &&\
                echo -n "    Mississippi    " && TZ=America\/Chicago date
            # pkg install `freecolor` -- memory usage
            echo 'memory usage' && freecolor
        }
        ##
        # `cmdwatch` is FreeBSD equivalent of Linux `watch`
        #
        # cmdwatch -n 60 'apm -l...'
        #
        # the advantage of using [cmd]watch is that there's no "blink" on screen clearing.
        # the disadvantage is that a command is needed, not a shell function
        # cmdwatch -n 60 'show_dashbd' fails.

        while true; do
            # https://stackoverflow.com/questions/5367068/clear-a-terminal-screen-for-real
            # todo: rm hardcoded vt100 escape code for portability,
            #    adding alias `cls` or somesuch for reuse in other scripts and functions.
            printf "\033c"
            show_dashbd
            sleep 60
        done
        # local function scoping proving difficult,
        # `unset -f` or equivalently `unfunction`
        # the "local" definition
        unset -f show_dashbd
        # todo: read about local definitions further.
        # todo: trap signals for cleaner exit
        # todo: Python port w/ this as bkup
    }


elif uname |grep -qi linux; then
    # todo: get more precise about uname if ever using linux again, portage
    alias dashbd='watch -n 60 "acpi ; TZ=America\/Los_Angeles date"'
    # todo: FreeBSD port && pkg_find_files function to prevent more such todos
    # alias pyhtmldoc='elinks /usr/share/doc/python3/html/index.html'
else
    # todo: portage if ever committing from Mac, etc.
    print -- "unknown OS!"
fi

# 💕☺

# -r is a test for readable files
if [[ -r ~/.aliasrc ]]; then
    . ~/.aliasrc
fi


## directory name shortcuts (wip)
# hash is a zsh builtin, and -d is for directory.
# `hash -d` allows creating shortcuts.
# use `cd ~<name>` e.g. `cd ~exercises`
# (with tab-completion) to change to specified directory.
# `hash -d` with no args to print all shortcuts.
function () {
    # todo: add zsh lang note "anon fn allows local variables" to cookbook
    local workspace_dir=ws
    hash -d music=~/music
    hash -d bkup=/media/bthumb
    hash -d workspace=~/${workspace_dir}
    hash -d go_almenac=~/${workspace_dir}/sand/go-almenac
    hash -d go_cat=~/${workspace_dir}/go/src/sand/go-catalog
    hash -d github=~/${workspace_dir}/github
    hash -d datamon=~/${workspace_dir}/github/datamon
    hash -d clj_news=~/${workspace_dir}/github/clj-news
    hash -d tub=~/${workspace_dir}/github/tubing
    hash -d exercises=~/${workspace_dir}/github/exercises
}

if [[ -r ~/.zsh_hashd ]]; then
    . ~/.zsh_hashd
fi



##


rand_int__1_to_5()
{
    if [[ "$(uname)" = "FreeBSD" ]]; then
	      random -f <(seq 1 5) |head -1
    else
        1>2 print -- '`random` is bsd-specific(?)'
        return 64
    fi
}

open_next_indexed_directory()
{
    indexed_dirs_str = $(find . -type d -maxdepth 1 |grep -v '^\.$' |sed 's/\.\///' | grep '^\d$' |sort)
    return 64
}

#####
