###########################################
#  .zprofile -- zsh startup script        #
#                                         #
# Author: Mathias Dannesbo <neic@neic.dk> #
# Time-stamp: <2012-08-16 14:23:47 (neic)>#
#                                         #
# Is sourced on login. startx on login at #
# tty1.                                   #
###########################################

PATH=/usr/local/texlive/2011/bin/x86_64-linux:$PATH
export PATH

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
  exec nohup startx > .xlog & vlock
fi
