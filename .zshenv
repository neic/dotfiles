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
    export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.7.0_51.jdk/Contents/Home"
elif (( $+commands[emacs] )); then
    export EDITOR=emacs
else
    export EDITOR=nano
fi

#------------------------------
# Misc
#------------------------------

if [ $(uname) = "Darwin" ]; then
    export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.7.0_51.jdk/Contents/Home"
fi

export HOMEBREW_NO_ANALYTICS="1"
