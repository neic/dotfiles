#------------------------------
# Locale
#------------------------------

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

#------------------------------
# Editor
#------------------------------

if [ $(uname) = "Darwin" ]; then
    export EDITOR="~/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -nw -a nano"
elif (( $+commands[emacs] )); then
    export EDITOR=emacs
else
    export EDITOR=nano
fi

#------------------------------
# Misc
#------------------------------

export HOMEBREW_NO_ANALYTICS="1"
export HOMEBREW_NO_INSTALL_CLEANUP="1"
