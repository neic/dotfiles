#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return
PS1='[\u@\h \W]\$ '

alias ls='ls --color=auto'
alias cle='sudo pacman -Rs $(pacman -Qqtd); sudo pacman -Sc'
alias up='sudo pacman -Syu'
alias wi='wicd-curses'
alias clear='echo "Use C-l to clear"'
alias exit='echo "Use C-d to exit"'

alias gs='git status'

bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

export EDITOR="/usr/bin/emacsclient -t"
