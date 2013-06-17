###########################################
#  .zprofile -- zsh startup script        #
#                                         #
# Author: Mathias Dannesbo <neic@neic.dk> #
# Time-stamp: <2013-05-27 13:11:33 (neic)>#
#                                         #
# Is sourced on login. startx on login at #
# tty1.                                   #
###########################################


if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
  exec nohup startx > .xlog & vlock
fi
