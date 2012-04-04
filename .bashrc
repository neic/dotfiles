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

alias ga='git add'
alias gp='git push'
alias gl='git log'
alias gs='git status'
alias gd='git diff'
alias gdc='git diff --cached'
alias gc='git commit'
alias gca='git commit -a'
alias gb='git branch'
alias gco='git checkout'
alias gra='git remote add'
alias grr='git remote rm'
alias gpu='git pull'
alias gcl='git clone'

bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

export EDITOR="/usr/bin/emacsclient -t"
