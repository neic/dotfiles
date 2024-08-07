# Locale
#-------

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# Path
#-----

# Nix changes the PATH in `/nix/store/<hash>-set-environment` via
# `/etc/zsh_env`. Homebrew changes the PATH in `.zprofile`.

# MacPorts
PATH="${PATH:+${PATH}:}/opt/local/bin:/opt/local/sbin:"
MANPATH="${MANPATH:+${MANPATH}:}/opt/local/share/man:"

if [[ ! "$PATH" == */usr/local/opt/fzf/bin* ]]; then
    PATH="${PATH:+${PATH}:}/usr/local/opt/fzf/bin"
fi

PATH="${PATH:+${PATH}:}${HOME}/.emacs.d/bin"
PATH="${PATH:+${PATH}:}${HOME}/Documents/bin"

# Editor
#-------

export EMACS_SOCKET_NAME=~/.emacs.d/server

if [ $(uname) = "Darwin" ]; then
    export EDITOR="emacsclient -nw -a nano"
else
    export EDITOR=nano
fi

# Homebrew
#---------

export HOMEBREW_NO_ANALYTICS="1"
export HOMEBREW_NO_INSTALL_CLEANUP="1"

# Docker
#-------

export COMPOSE_DOCKER_CLI_BUILD="1"
export DOCKER_BUILDKIT="1"

# Kubernetes
#-----------

export USE_GKE_GCLOUD_AUTH_PLUGIN=True
export K9SCONFIG=~/.config/k9s

# Kyrat
#------

export KYRAT_SHELL=zsh
