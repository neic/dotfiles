###########################################
#  .zshrc -- zsh resource file            #
#                                         #
# Author: Mathias Dannesbo <neic@neic.dk> #
# Time-stamp: <2016-01-20 11:01:15 (neic)>#
#                                         #
# Is sourced if interactive.              #
###########################################

set noclobber

#------------------------------
# History stuff
#------------------------------
export HISTFILE=~/.zsh_history
export HISTSIZE=12000
export SAVEHIST=10000

setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.

if [ -n "${commands[fzf-share]}" ]; then
  source "$(fzf-share)/key-bindings.zsh"
fi

bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward

#-----------------------------
# Colors
#-----------------------------

# load some modules
autoload -U zsh/terminfo # Used in the colour alias below
if autoload colors && colors 2>/dev/null ; then
    BLUE="%{${fg[blue]}%}"
    RED="%{${fg_bold[red]}%}"
    GREEN="%{${fg[green]}%}"
    CYAN="%{${fg[cyan]}%}"
    MAGENTA="%{${fg[magenta]}%}"
    YELLOW="%{${fg[yellow]}%}"
    WHITE="%{${fg[white]}%}"
    NO_COLOR="%{${reset_color}%}"
else
    BLUE=$'%{\e[1;34m%}'
    RED=$'%{\e[1;31m%}'
    GREEN=$'%{\e[1;32m%}'
    CYAN=$'%{\e[1;36m%}'
    WHITE=$'%{\e[1;37m%}'
    MAGENTA=$'%{\e[1;35m%}'
    YELLOW=$'%{\e[1;33m%}'
    NO_COLOR=$'%{\e[0m%}'
fi

#------------------------------
# Completion
#------------------------------

# Add current nix system to ZSH path to enable completion of installed packages.
# This is likely a upstream bug, but I haven't investigated. See
# https://github.com/nix-community/home-manager/issues/2802
fpath+=(/run/current-system/sw/share/zsh/site-functions)

zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' 'r:|[._-]=* r:|=*'
zstyle ':completion:*' menu select=long

autoload -Uz compinit
compinit

autoload -Uz bashcompinit
bashcompinit

complete -C terraform terraform

if [ -n "${commands[fzf-share]}" ]; then
  source "$(fzf-share)/completion.zsh"
fi

#------------------------------
# Aliases
#------------------------------

# GNU coreutils in homebrew is with a `g`-prefix. Remove this when running nix
# everywhere.
if [ -n "${commands[gls]}" ]; then
  alias ls='gls --color=always --classify'
  alias ll='gls --color=always -l'
else
  alias ls='ls --color=always --classify'
  alias ll='ls --color=always -l'
fi


alias gitlab-run='docker run --rm -v $PWD:$PWD -v /var/run/docker.sock:/var/run/docker.sock --workdir $PWD gitlab/gitlab-runner exec docker'

cle () {
    if [ $(uname) = "Darwin" ]; then
        print -P "${BLUE}Cleaning homebrew software${NO_COLOR}"
        brew cleanup
    elif [[ $(uname) = "Linux" && $(lsb_release -si) = "Ubuntu" ]]; then
        print -P "${BLUE}Cleaning software from apt-get${NO_COLOR}"
        sudo apt-get autoremove
        sudo apt-get autoclean
    elif [[ $(uname) = "Linux" && $(lsb_release -si) = "Arch" ]]; then
        print -P "${BLUE}Cleaning software from pacman${NO_COLOR}"
        sudo pacman -Rs $(pacman -Qqtd); sudo pacman -Sc
    else
        print "Cleaning failed: OS is not OSX, Ubuntu or Arch"
    fi
}

up () {
    if [ $(uname) = "Darwin" ]; then
        print -P "${BLUE}Updating OSX and App Store software${NO_COLOR}"
        sudo softwareupdate --install --all
        print -P "${BLUE}Updating homebrew software${NO_COLOR}"
        brew update
        brew upgrade
        if (( $+commands[nix-channel] )); then
            print -P "${BLUE}Updating nix${NO_COLOR}"
            nix-channel --update
            darwin-rebuild switch
        fi
    elif [[ $(uname) = "Linux" && $(lsb_release -si) = "Ubuntu" ]]; then
        print -P "${BLUE}Updating software from apt-get${NO_COLOR}"
        sudo apt-get update
        sudo apt-get upgrade
        if [ -f /var/run/reboot-required ]; then
            print -P "${YELLOW}Reboot required${NO_COLOR}"
        fi
    elif [[ $(uname) = "Linux" && $(lsb_release -si) = "Arch" ]]; then
        print -P "${BLUE}Updating software from pacman${NO_COLOR}"
        sudo pacman -Syu
    else
        print "Updating failed: OS is not OSX, Ubuntu or Arch"
    fi

    if (( $+commands[doom] )); then
        print -P "${BLUE}Updating Doom Emacs${NO_COLOR}"
        doom upgrade
        doom sync
    fi

    if (( $+commands[tlmgr] )); then
        print -P "${BLUE}Updating TeX Live${NO_COLOR}"
        sudo tlmgr update --self
        sudo tlmgr update --all
    fi
}

pipup () {
    pip3 --version
    pip3 install --upgrade pip
    pip3 freeze --local | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 $1 install -U
}

emacs () {
    if [ $(uname) = "Darwin" ]; then
        /usr/local/bin/emacs "$@"
    else
        command emacs
    fi
}

ec () {
    if [ -z "$1" ]; then
        TMP="$(mktemp /tmp/stdin-XXX)"
        cat >$TMP
        emacsclient -n $TMP
        rm $TMP
    else
        emacsclient -n "$@"
    fi
}

gs () {
    if (( $+commands[emacsclient] )); then
        emacsclient -e "(magit-status \"$(git rev-parse --show-toplevel)\")" > /dev/null
        emacsclient -e "(select-frame-set-input-focus (selected-frame))" > /dev/null
    else
        git status
    fi
}

function prune_local_branches () {
    # Deletes branches locally that no longer exist on remote
    # (eg. after merge + "Delete source branch")

    # The script works by checking out the trunk branch,
    # pulling the latest changes from remote with the prune
    # flag enabled and finally deleting branches that are
    # marked as "gone".

    # You can (and should) use this script gratuitously
    # as it has the added benefit of making sure your trunk
    # is up-to-date when you start new feature branches.

    # Generally, the script can be run as is; specifying
    # a target branch is only needed if you don't want to
    # use development or master as your trunk branch.

    # Usage:
    ## prune_local_branches <optional trunk branch name>
    BRANCH_NAME="${1:-development}"
    # Verify that target branch exists, fall back to master or main
    if ! $(git rev-parse --verify --quiet "$BRANCH_NAME" >/dev/null); then
        BRANCH_NAME=master
    fi
    if ! $(git rev-parse --verify --quiet "$BRANCH_NAME" >/dev/null); then
        BRANCH_NAME=main
    fi
    if ! $(git rev-parse --verify --quiet "$BRANCH_NAME" >/dev/null); then
        echo "Branch does not exists."
        exit 1
    fi
    git checkout "$BRANCH_NAME"
    git pull -p
    git branch -vv | grep ': gone]' | awk '{print $1}' | xargs git branch -D
    git branch --merged | egrep -v "(^\*|master|main|dev)" | xargs git branch -d
}


alias notify='terminal-notifier -title "Terminal" -message "Done with task! Exit status: $?" -activate com.apple.Terminal'


#------------------------------
# Prompt
#------------------------------

setopt prompt_subst

# User part
eval PR_USER='${GREEN}%n${NO_COLOR}'
eval PR_USER_OP='${GREEN}%#${NO_COLOR}'

if [[ $UID -eq 0 ]]; then # root
    eval PR_USER='${RED}%n${NO_COLOR}'
    eval PR_USER_OP='${RED}%#${NO_COLOR}'
fi

# Host part
eval PR_HOST='${GREEN}%M${NO_COLOR}'

if [[ -n "$SSH_CLIENT" || -n "$SSH2_CLIENT" ]]; then
    eval PR_HOST='${YELLOW}%M${NO_COLOR}'
fi

if [[ "$HOST" = "jump" ]]; then
    eval PR_HOST='${RED}%M${NO_COLOR}'
fi

# Return code
eval PR_RET='%(?..${RED}%?${NO_COLOR} )'

# set the prompt
PS1=$'${PR_RET}${CYAN}[${PR_USER}${CYAN}@${PR_HOST}${CYAN}][${BLUE}%~${CYAN}]${PR_USER_OP} '
PS2=$'%_>'
RPROMPT='$(date +%T)'

#------------------------------
# Dumb terminal
#------------------------------

# Overwrite fancy setting if the terminal is dumb. Emacs is dumb.
if [[ "$TERM" == "dumb" ]]
then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
    unalias ls
    unalias ll
    PS1='$ '
fi
