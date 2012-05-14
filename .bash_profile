#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc
PATH=/usr/local/texlive/2011/bin/x86_64-linux:$PATH
export PATH

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
  exec nohup startx > .xlog & vlock
fi
