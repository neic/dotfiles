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
# Homebrew
#------------------------------

export HOMEBREW_NO_ANALYTICS="1"
export HOMEBREW_NO_INSTALL_CLEANUP="1"

#------------------------------
# Docker
#------------------------------

export COMPOSE_DOCKER_CLI_BUILD="1"
export DOCKER_BUILDKIT="1"

#------------------------------
# kubectl
#------------------------------

export USE_GKE_GCLOUD_AUTH_PLUGIN=True
