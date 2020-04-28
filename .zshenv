#------------------------------
# Locale
#------------------------------

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

#------------------------------
# Editor
#------------------------------

if [ $(uname) = "Darwin" ]; then
    export EDITOR="emacsclient -nw -a nano"
else
    export EDITOR=nano
fi

#------------------------------
# Misc
#------------------------------

export HOMEBREW_NO_ANALYTICS="1"
export HOMEBREW_NO_INSTALL_CLEANUP="1"
