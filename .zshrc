###########################################
#  .zshrc -- zsh resource file            #
#                                         #
# Author: Mathias Dannesbo <neic@neic.dk> #
# Time-stamp: <2013-11-11 22:55:23 (neic)>#
#                                         #
# Is sourced if interactive.              #
###########################################

export EDITOR="/usr/bin/emacsclient -t"
export PATH=/usr/local/bin:${PATH}
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
autoload -U compinit
compinit

zstyle ':completion:*' menu select
setopt completealiases

#------------------------------
# Keybingings
#------------------------------

bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward

#------------------------------
# Aliases
#------------------------------

alias ll='ls -l'
alias cle='sudo pacman -Rs $(pacman -Qqtd); sudo pacman -Sc'
alias up='sudo pacman -Syu'
alias wi='wicd-curses'
alias clear='echo "Use C-l to clear"'
alias exit='echo "Use C-d to exit"'

alias gs='git status'

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

alias lock='alock -auth pam -bg blank'
alias suspend='lock & sudo pm-suspend'

#------------------------------
# Prompt
#------------------------------

# load some modules
autoload -U colors zsh/terminfo # Used in the colour alias below
colors
setopt prompt_subst

# make some aliases for the colours: (coud use normal escap.seq's too)
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE GREY; do
    eval PR_$color='%{$fg[${(L)color}]%}'
done
PR_NO_COLOR="%{$terminfo[sgr0]%}"

# Check the UID
if [[ $UID -ge 1000 ]]; then # normal user
    eval PR_USER='${PR_GREEN}%n${PR_NO_COLOR}'
    eval PR_USER_OP='${PR_GREEN}%#${PR_NO_COLOR}'
elif [[ $UID -eq 0 ]]; then # root
    eval PR_USER='${PR_RED}%n${PR_NO_COLOR}'
    eval PR_USER_OP='${PR_RED}%#${PR_NO_COLOR}'
fi

# Check if we are on SSH or not
if [[ -n "$SSH_CLIENT" || -n "$SSH2_CLIENT" ]]; then
    eval PR_HOST='${PR_YELLOW}%M${PR_NO_COLOR}' #SSH
else
    eval PR_HOST='${PR_GREEN}%M${PR_NO_COLOR}' # no SSH
fi
 
eval PR_RET='%(?..${PR_RED}%?${PR_NO_COLOR} )'

# set the prompt
PS1=$'${PR_RET}${PR_CYAN}[${PR_USER}${PR_CYAN}@${PR_HOST}${PR_CYAN}][${PR_BLUE}%~${PR_CYAN}]${PR_USER_OP} '
PS2=$'%_>'

RPROMPT='$(date +%T)'

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
