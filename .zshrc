###########################################
#  .zshrc -- zsh resource file            #
#                                         #
# Author: Mathias Dannesbo <neic@neic.dk> #
# Time-stamp: <2014-06-08 16:53:59 (neic)>#
#                                         #
# Is sourced if interactive.              #
###########################################

export EDITOR="emacsclient -nw -a nano"
export GOPATH=$HOME/Documents/go
export PATH=/usr/local/bin:${PATH}:$GOPATH/bin
export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.7.0_51.jdk/Contents/Home"
set noclobber

#------------------------------
# History stuff
#------------------------------
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000

#-----------------------------
# Colors
#-----------------------------
LS_COLORS='rs=0:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32:';
export LS_COLORS

export ANT_ARGS='-logger org.apache.tools.ant.listener.AnsiColorLogger'

#------------------------------
# Completion
#------------------------------

# From grml

# allow one error for every three characters typed in approximate completer
zstyle ':completion:*:approximate:'    max-errors 'reply=( $((($#PREFIX+$#SUFFIX)/3 )) numeric )'

# don't complete backup files as executables
zstyle ':completion:*:complete:-command-::commands' ignored-patterns '(aptitude-*|*\~)'

# start menu completion only if it could find no unambiguous initial string
zstyle ':completion:*:correct:*'       insert-unambiguous true
zstyle ':completion:*:corrections'     format $'%{\e[0;31m%}%d (errors: %e)%{\e[0m%}'
zstyle ':completion:*:correct:*'       original true

# activate color-completion
zstyle ':completion:*:default'         list-colors ${(s.:.)LS_COLORS}

# format on completion
zstyle ':completion:*:descriptions'    format $'%{\e[0;31m%}completing %B%d%b%{\e[0m%}'

# automatically complete 'cd -<tab>' and 'cd -<ctrl-d>' with menu
# zstyle ':completion:*:*:cd:*:directory-stack' menu yes select

# insert all expansions for expand completer
zstyle ':completion:*:expand:*'        tag-order all-expansions
zstyle ':completion:*:history-words'   list false

# activate menu
zstyle ':completion:*:history-words'   menu yes

# ignore duplicate entries
zstyle ':completion:*:history-words'   remove-all-dups yes
zstyle ':completion:*:history-words'   stop yes

# match uppercase from lowercase
zstyle ':completion:*'                 matcher-list 'm:{a-z}={A-Z}'

# separate matches into groups
zstyle ':completion:*:matches'         group 'yes'
zstyle ':completion:*'                 group-name ''

if [[ "$NOMENU" -eq 0 ]] ; then
    # if there are more than 5 options allow selecting from a menu
    zstyle ':completion:*'               menu select=5
else
    # don't use any menus at all
    setopt no_auto_menu
fi

zstyle ':completion:*:messages'        format '%d'
zstyle ':completion:*:options'         auto-description '%d'

# describe options in full
zstyle ':completion:*:options'         description 'yes'

# on processes completion complete all user processes
zstyle ':completion:*:processes'       command 'ps -au$USER'

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# provide verbose completion information
zstyle ':completion:*'                 verbose true

# recent (as of Dec 2007) zsh versions are able to provide descriptions
# for commands (read: 1st word in the line) that it will list for the user
# to choose from. The following disables that, because it's not exactly fast.
zstyle ':completion:*:-command-:*:'    verbose false

# set format for warnings
zstyle ':completion:*:warnings'        format $'%{\e[0;31m%}No matches for:%{\e[0m%} %d'

# define files to ignore for zcompile
zstyle ':completion:*:*:zcompile:*'    ignored-patterns '(*~|*.zwc)'
zstyle ':completion:correct:'          prompt 'correct to: %e'

# Ignore completion functions for commands you don't have:
zstyle ':completion::(^approximate*):*:functions' ignored-patterns '_*'

# Provide more processes in completion of programs like killall:
zstyle ':completion:*:processes-names' command 'ps c -u ${USER} -o command | uniq'

# complete manual by their section
zstyle ':completion:*:manuals'    separate-sections true
zstyle ':completion:*:manuals.*'  insert-sections   true
zstyle ':completion:*:man:*'      menu yes select

# Search path for sudo completion
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin \
    /usr/local/bin  \
    /usr/sbin       \
    /usr/bin        \
    /sbin           \
    /bin            \
    /usr/X11R6/bin

# provide .. as a completion
zstyle ':completion:*' special-dirs ..

# run rehash on completion so new installed program are found automatically:
_force_rehash() {
    (( CURRENT == 1 )) && rehash
    return 1
}

## correction
# some people don't like the automatic correction - so run 'NOCOR=1 zsh' to deactivate it
if [[ "$NOCOR" -gt 0 ]] ; then
    zstyle ':completion:*' completer _oldlist _expand _force_rehash _complete _files _ignored
    setopt nocorrect
else
    # try to be smart about when to use what completer...
    setopt correct
    zstyle -e ':completion:*' completer '
            if [[ $_last_try != "$HISTNO$BUFFER$CURSOR" ]] ; then
                _last_try="$HISTNO$BUFFER$CURSOR"
                reply=(_complete _match _ignored _prefix _files)
            else
                if [[ $words[1] == (rm|mv) ]] ; then
                    reply=(_complete _files)
                else
                    reply=(_oldlist _expand _force_rehash _complete _ignored _correct _approximate _files)
                fi
            fi'
fi

# command for process lists, the local web server details and host completion
zstyle ':completion:*:urls' local 'www' '/var/www/' 'public_html'

# caching
[[ -d $ZSHDIR/cache ]] && zstyle ':completion:*' use-cache yes && \
    zstyle ':completion::complete:*' cache-path $ZSHDIR/cache/

# host completion
[[ -r ~/.ssh/known_hosts ]] && _ssh_hosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[\|]*}%%\ *}%%,*}) || _ssh_hosts=()
[[ -r /etc/hosts ]] && : ${(A)_etc_hosts:=${(s: :)${(ps:\t:)${${(f)~~"$(</etc/hosts)"}%%\#*}##[:blank:]#[^[:blank:]]#}}} || _etc_hosts=()

hosts=(
    $(hostname)
    "$_ssh_hosts[@]"
    "$_etc_hosts[@]"
    localhost
)
zstyle ':completion:*:hosts' hosts $hosts
# TODO: so, why is this here?
#  zstyle '*' hosts $hosts

# end grml

autoload -Uz compinit
compinit -d

#------------------------------
# Keybingings
#------------------------------

bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward

#------------------------------
# Aliases
#------------------------------

alias ll='ls -l'
alias clear='echo "Use C-l to clear"'
alias exit='echo "Use C-d to exit"'

alias gs='git status'

cle () {
    if [ $(uname) = "Darwin" ]; then
        print "Cleaning homebrew software"
        brew cleanup
    elif [ $(uname) = "Linux" ]; then
        print "Cleaning software from apt-get"
        sudo apt-get autoremove
        sudo apt-get autoclean
    else
        print "Cleaning failed: OS is not OSX or debian-based"
    fi
}

up () {
    if [ $(uname) = "Darwin" ]; then
        print "Updating OSX and App Store software"
        softwareupdate -ia
        print "Updating homebrew software"
        brew update
        brew upgrade
    elif [ $(uname) = "Linux" ]; then
        print "Updating software from apt-get"
        sudo apt-get update
        sudo apt-get upgrade
    else
        print "Updating failed: OS is not OSX or debian-based"
    fi

    if [ $+commands[tlmgr] ]; then
        print "Updating TeX Live"
        sudo tlmgr update --self
        sudo tlmgr update --all
    fi
}

emacs () {
    if [ $(uname) = "Darwin" ]; then
        /Applications/Emacs.app/Contents/MacOS/Emacs "$@"
    else
        command emacs
    fi
}

ec () {
    if [ $(uname) = "Darwin" ]; then
        open -a /Applications/Emacs.app "$@"
    else
        emacsclient
    fi
}
#------------------------------
# Power management
#------------------------------

function savevm {
    RUNNINGVMS=("${(@f)$(vboxmanage list runningvms | grep -o -P '(?<=[{]).*(?=[}])')}")
    if [ -n '$RUNNINGVMS' ]; then
        for VM in $RUNNINGVMS; do
            vboxmanage controlvm $VM savestate
        done
    fi
}

#-----------------------------
# Colors
#-----------------------------

if autoload colors && colors 2>/dev/null ; then
    BLUE="%{${fg[blue]}%}"
    RED="%{${fg_bold[red]}%}"
    GREEN="%{${fg[green]}%}"
    CYAN="%{${fg[cyan]}%}"
    MAGENTA="%{${fg[magenta]}%}"
    YELLOW="%{${fg[yellow]}%}"
    WHITE="%{${fg[white]}%}"
    NO_COLOR="%{${reset_color}%}"
else
    BLUE=$'%{\e[1;34m%}'
    RED=$'%{\e[1;31m%}'
    GREEN=$'%{\e[1;32m%}'
    CYAN=$'%{\e[1;36m%}'
    WHITE=$'%{\e[1;37m%}'
    MAGENTA=$'%{\e[1;35m%}'
    YELLOW=$'%{\e[1;33m%}'
    NO_COLOR=$'%{\e[0m%}'
fi

#LS_COLORS='rs=0:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32:';
export LS_COLORS

export ANT_ARGS='-logger org.apache.tools.ant.listener.AnsiColorLogger'

#------------------------------
# Prompt
#------------------------------

# load some modules
autoload -U zsh/terminfo # Used in the colour alias below
setopt prompt_subst

# make some aliases for the colours: (coud use normal escap.seq's too)

# Check the UID
if [[ $UID -ge 1000 ]]; then # normal user
    eval PR_USER='${GREEN}%n${NO_COLOR}'
    eval PR_USER_OP='${GREEN}%#${NO_COLOR}'
elif [[ $UID -eq 0 ]]; then # root
    eval PR_USER='${RED}%n${NO_COLOR}'
    eval PR_USER_OP='${RED}%#${NO_COLOR}'
fi

# Check if we are on SSH or not
if [[ -n "$SSH_CLIENT" || -n "$SSH2_CLIENT" ]]; then
    eval PR_HOST='${YELLOW}%M${NO_COLOR}' #SSH
else
    eval PR_HOST='${GREEN}%M${NO_COLOR}' # no SSH
fi

eval PR_RET='%(?..${RED}%?${NO_COLOR} )'

# set the prompt
case $TERM in
    "dumb")
        PS1="> "
        ;;
    termite|*xterm*|rxvt|rxvt-unicode|rxvt-256color|rxvt-unicode-256color|screen|(dt|k|E)term)
    PS1=$'${PR_RET}${CYAN}[${PR_USER}${CYAN}@${PR_HOST}${CYAN}][${BLUE}%~${CYAN}]${PR_USER_OP} '
    PS2=$'%_>'
    RPROMPT='$(date +%T)'
    ;;
    *)
        PS1="> "
        ;;
esac
#------------------------------
# Window title
#------------------------------
case $TERM in
    termite|*xterm*|rxvt|rxvt-unicode|rxvt-256color|rxvt-unicode-256color|(dt|k|E)term)
    precmd () { print -Pn "\e]0;[%n@%M][%~]%#\a" }
    preexec () { print -Pn "\e]0;[%n@%M][%~]%# ($1)\a" }
    ;;
    screen)
        precmd () {
            print -Pn "\e]83;title \"$1\"\a"
            print -Pn "\e]0;$TERM - (%L) [%n@%M]%# [%~]\a"
        }
        preexec () {
            print -Pn "\e]83;title \"$1\"\a"
            print -Pn "\e]0;$TERM - (%L) [%n@%M]%# [%~] ($1)\a"
        }
        ;;
esac
