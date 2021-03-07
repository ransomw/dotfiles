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
# in addition to the `%>>` truncation operator and colors,
# this prompt uses the `%(?..` ternary operator to display
# red or green depending on the last command's exit code.
PS1='%10>...>'"%(?.%{${fg[green]}%}.%{${fg[red]}%})"'%1/'"%{$reset_color%}"'%>>%# '

# `typeset -U` ensures uniqueness of elements in an array:
# duplicate elements after the first (from the left) are removed
typeset -U path
# and zsh auto-converts the $path array and $PATH env var
path=(~/bin $path)

## language environments
# pyenv (https://github.com/pyenv/pyenv)
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init -)"
fi

export GOPATH="$HOME/ws/go"

## VPSs
LISA=45.55.5.68
MARGE=192.241.218.140

## aliases

# todo: use .aliases alias to .dotfiles,
#    combine these with existing bash_aliases.sh,
#    and build aliases by os and shell.
alias m='mbsync gmail-fetch && mbsync gmail-fetch-sent && mbsync mutt-fetch && mbsync mutt-fetch-sent && mutt'

# -h for human-readable file sizes
alias ls='ls -h'
# sort by descending creation date
alias ls_time='ls -ct'

##
# prevent man page line-wrap by fitting page
# to terminal
#
# $COLUMNS is a zsh "magic" variable
# the MANWIDTH env var is tested on FreeBSD..
# .. does this variable have an effect on all systems?
alias man='MANWIDTH=$COLUMNS man'

alias mpv='/usr/local/bin/mpv --audio-device=oss//dev/dsp0.0'
alias mn='mpv --vid=no'
alias mpvm='mpv -mute="yes"'
alias mpvs='mpv --shuffle'

alias ddg='links http://www.duckduckgo.com'
alias handbook='links /usr/local/share/doc/freebsd/handbook/index.html'


alias emacs='emacs -nw --no-desktop'
# todo: debug, elaborating to function as needed
alias xemacs='emacsclient -c || (emacs --daemon --no-desktop && sleep 2.3 && emacsclient -c)'



# -r is a test for readable files
if [[ -r ~/.aliasrc ]]; then
    . ~/.aliasrc
fi


## shortcuts
workspace_dir=ws
hash -d bkup=/media/bthumb
hash -d workspace=~/${workspace_dir}
hash -d go_almenac=~/${workspace_dir}/sand/go-almenac
hash -d go_cat=~/${workspace_dir}/go/src/sand/go-catalog
hash -d github=~/${workspace_dir}/github
hash -d datamon=~/${workspace_dir}/github/datamon
hash -d clj_news=~/${workspace_dir}/github/clj-news
hash -d tub=~/${workspace_dir}/github/tubing
hash -d exercises=~/${workspace_dir}/github/exercises

if [[ -r ~/.zsh_hashd ]]; then
    . ~/.zsh_hashd
fi


## python env setup

if [[ -f ~/.pythonrc ]]; then
    export PYTHONSTARTUP="${HOME}/.pythonrc"
fi


##


## user-defined path extensions
export PATH="$HOME/bin:$PATH"

export PLAN9=/usr/home/ransom/sbin/plan9
export PATH=$PATH:$PLAN9/bin
alias acme='acme -f $PLAN9/font/fixed/unicode.10x20.font'


#####
