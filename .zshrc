###########################################
#  .zshrc -- zsh resource file            #
#                                         #
# Author: Mathias Dannesbo <neic@neic.dk> #
# Time-stamp: <2016-01-20 11:01:15 (neic)>#
#                                         #
# Is sourced if interactive.              #
###########################################

#------------------------------
# Oh-my-zsh setup
#------------------------------

# Path to your oh-my-zsh installation.
  export ZSH="/Users/neic/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="pygmalion"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git
    osx
    docker
)

source $ZSH/oh-my-zsh.sh



#-----------------------------
# Colors
#-----------------------------

export ANT_ARGS='-logger org.apache.tools.ant.listener.AnsiColorLogger'


#------------------------------
# Aliases
#------------------------------
if [ $(uname) = "Darwin" ]; then
    alias ls='gls -C -F --color=always'
fi

alias ll='ls -l'
alias clear='echo "Use C-l to clear"'
alias exit='echo "Use C-d to exit"'

alias gs='git status'

sshvm () {
    if [[ $1 =~ "@" ]]; then
        NAME=$(echo $1 | perl -nle'print $1 if /\@(.*)/')
        USERN=$(echo $1 | perl -nle'print $1 if /(.*)\@/')
    else
        NAME=${1}
        USERN=$USERNAME
    fi

    if [[ $(VBoxManage list runningvms) =~ $NAME ]]; then
        print $NAME "is already running. SSHing..."
    else
        VBoxManage startvm $NAME --type headless
    fi

    if [ $? -eq 0 ]; then
        PORT=$(VBoxManage showvminfo $NAME --details | grep '^NIC.*localssh' | perl -nle'print $1 if /host port =.*?(\d+)/')
        if [[ ! -z $PORT ]]; then
            ssh -l $USERN -p $PORT $2 localhost
        else
            print "The port can't be parsed. Remember to port forward and call the rule 'localssh'."
            return 1
        fi
    else
        print "The VM" $1 "is not known."
        return 1
    fi
}

cle () {
    if [ $(uname) = "Darwin" ]; then
        print "Cleaning homebrew software"
        brew cleanup
    elif [[ $(uname) = "Linux" && $(lsb_release -si) = "Ubuntu" ]]; then
        print "Cleaning software from apt-get"
        sudo apt-get autoremove
        sudo apt-get autoclean
    elif [[ $(uname) = "Linux" && $(lsb_release -si) = "Arch" ]]; then
        print "Cleaning software from pacman"
        sudo pacman -Rs $(pacman -Qqtd); sudo pacman -Sc
    else
        print "Cleaning failed: OS is not OSX, Ubuntu or Arch"
    fi
}

up () {
    if [ $(uname) = "Darwin" ]; then
        print "Updating OSX and App Store software"
        sudo softwareupdate -ia
        print "Updating homebrew software"
        brew update
        brew upgrade
    elif [[ $(uname) = "Linux" && $(lsb_release -si) = "Ubuntu" ]]; then
        print "Updating software from apt-get"
        sudo apt-get update
        sudo apt-get upgrade
    elif [[ $(uname) = "Linux" && $(lsb_release -si) = "Arch" ]]; then
        print "Updating software from pacman"
        sudo pacman -Syu
    else
        print "Updating failed: OS is not OSX, Ubuntu or Arch"
    fi

    if (( $+commands[tlmgr] )); then
        print "Updating TeX Live"
        sudo tlmgr update --self
        sudo tlmgr update --all
    fi
}

pipup () {
    pipupinner pip
}

pip2up () {
    pipupinner pip2
}

pip3up () {
    pipupinner pip3
}

function pipupinner() {
    $1 --version
    $1 install --upgrade pip
    $1 freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 $1 install -U
}

emacs () {
    if [ $(uname) = "Darwin" ]; then
        /usr/local/bin/emacs "$@"
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

enableproxy () {
    ssh -Nf -D 9001 ${1:=wiply}
    sudo networksetup -setsocksfirewallproxy "Wi-Fi" localhost 9001
    sudo networksetup -setsocksfirewallproxy "Thunderbolt Ethernet" localhost 9001
    sudo networksetup -setsocksfirewallproxy "iPhone USB" localhost 9001
}


disableproxy () {
    sudo networksetup -setsocksfirewallproxystate "Wi-Fi" off
    sudo networksetup -setsocksfirewallproxystate "Thunderbolt Ethernet" off
    sudo networksetup -setsocksfirewallproxystate "iPhone USB" off
}
