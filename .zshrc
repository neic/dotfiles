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

#-----------------------------
# Colors
#-----------------------------

# load some modules
autoload -U zsh/terminfo # Used in the colour alias below
if autoload colors && colors 2>/dev/null ; then
    BLUE="%{${fg[blue]}%}" # $CWD
    RED="%{${fg[red]}%}" # Exitcode, context errors, root username
    GREEN="%{${fg[green]}%}" # Docker context
    CYAN="%{${fg[cyan]}%}" # K8s context
    MAGENTA="%{${fg[magenta]}%}" # Nix-shell context
    YELLOW="%{${fg[yellow]}%}" # Terraform context, other usernames, hostname
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
compinit -u

autoload -Uz bashcompinit
bashcompinit

complete -C terraform terraform

if [ -n "${commands[fzf-share]}" ]; then
  source "$(fzf-share)/completion.zsh"
fi

#------------------------------
# Aliases
#------------------------------

alias ls='ls --color=always --classify'
alias ll='ls --color=always -l'

alias sa='SADMIN_PASS="$(pass scalgo/sadmin.scalgo.com | head -n1)" sadmin auth'
alias dive='DOCKER_HOST="$(docker context inspect --format='{{.Endpoints.docker.Host}}')" dive'

ksh () {
  # ssh to a host using kyrat to bring along the dotfiles, starts a screen and
  # sets the window/tab title to the hostname.

  echo -e "\e];$1\e\\" # Note the last character is ESC \, the String terminator
                       # C1 control code, not a double escaped double quote.
  kyrat $1 -- screen -DR
  echo -e "\e];\e\\"
}

if [[ -n $KYRAT_HOME ]]; then
  alias sudo='sudo --preserve-env=ZDOTDIR ONFOREIGNHOST=true'
fi

cle () {
    if [ $(uname) = "Darwin" ]; then
        print -P "${BLUE}Cleaning homebrew software${NO_COLOR}"
        brew cleanup
    elif [[ $(uname) = "Linux" && $(lsb_release -si) = "Ubuntu" ]]; then
        print -P "${BLUE}Cleaning software from apt-get${NO_COLOR}"
        sudo apt-get autoremove
        sudo apt-get autoclean
    else
        print "Cleaning failed: OS is not macOS or Ubuntu"
    fi

    if (( $+commands[nix-collect-garbage] )); then
        print -P "${BLUE}Cleaning nix${NO_COLOR}"
        nix-collect-garbage --delete-older-than 30d
    fi
}

up () {
    if [ $(uname) = "Darwin" ]; then
        print -P "${BLUE}Updating OSX and App Store software${NO_COLOR}"
        sudo softwareupdate --install --all
        if (( $+commands[nix-channel] )); then
            print -P "${BLUE}Updating nix${NO_COLOR}"
            sudo -i nix-channel --update
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
    else
        print "Updating failed: OS is not macOS or Ubuntu"
    fi

    if (( $+commands[doom] )); then
        print -P "${BLUE}Updating Doom Emacs${NO_COLOR}"
        doom upgrade
        doom sync
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
    git branch -vv | grep ': gone]' | awk '{print $1}' | xargs --no-run-if-empty git branch -D
    git branch --merged | egrep -v "(^\*|master|main|dev)" | xargs --no-run-if-empty git branch -d
}

flushdns () {
    if [ $(uname) = "Darwin" ]; then
        sudo dscacheutil -flushcache
        sudo killall -HUP mDNSResponder
    else
        print "Cleaning failed: OS is not macOS"
    fi
}
if [[ -f ~/.zshplugins/zsh-nix-shell/nix-shell.plugin.zsh ]]; then
    source ~/.zshplugins/zsh-nix-shell/nix-shell.plugin.zsh
fi

#------------------------------
# Prompt
#------------------------------

if [ -n "${functions[prompt]}" ]; then
  prompt off
fi
setopt prompt_subst

# User and host
if [[ $UID -eq 0 ]]; then # root
    eval PR_USER='${RED}%n'
    eval PR_USER_OP='${RED}%#'
elif [[ ! ($USER -eq 'neic' || $USER -eq 'md') ]]; then
    eval PR_USER='${YELLOW}%n'
    eval PR_USER_OP='${YELLOW}%#'
else
    eval PR_USER_OP='${WHITE}%#'
fi

if [[ -n "$SSH_CLIENT" || -n "$SSH2_CLIENT" || -n "$ONFOREIGNHOST" ]]; then
  eval PR_HOST='${YELLOW}%M${NO_COLOR}'
fi

if [[ -n "$PR_USER" || -n "$PR_HOST" ]]; then
  eval PR_LOGIN='${PR_USER}${WHITE}@${PR_HOST}\ '
fi

# Contexts
turn_on_contexts() {
  local current_cmd=$(echo $1 | cut -d' ' -f1)

  if [[ "$current_cmd" =~ ^"kubectl|flux|k9s|kubectx" ]]; then
      export CTX_KUBE=true
  elif [[ "$current_cmd" =~ ^"terraform" ]]; then
      export CTX_TF=true
  elif [[ "$current_cmd" =~ ^"colima|docker|docker-compose|podman" ]]; then
      export CTX_DO=true
  fi
}
preexec_functions+=(turn_on_contexts)

set_contexts() {
    if [[ -v IN_NIX_SHELL ]]; then
        eval PR_NIX='${MAGENTA}󱄅${IN_NIX_SHELL:0:1}\(${NO_COLOR}${NIX_SHELL_PACKAGES}${MAGENTA}\)\ '
    fi
    if [[ $CTX_KUBE ]]; then
      if command kubectl config current-context &> /dev/null; then
        eval PR_KUBE='${CYAN}󱃾\ $(command kubectl config current-context)\ '
      else
        eval PR_KUBE='${RED}󱃾\ no\ context\ '
      fi
    fi
    if [[ $CTX_TF ]]; then
        eval PR_TF='${YELLOW}󱁢\ $(command terraform workspace show)\ '
    fi
    if [[ $CTX_DO ]]; then
      if command colima status &> /dev/null || command docker info &> /dev/null; then
        eval PR_DO='${GREEN}󰡨󰐊\ '
      else
        eval PR_DO='${RED}󰡨󰓛\ '
      fi
    fi
}
precmd_functions+=(set_contexts)

# Return code
eval PR_RET='%(?..${RED}%?${NO_COLOR} )'

# set the prompt
PS1=$'${PR_RET}${PR_LOGIN}${PR_NIX}${PR_KUBE}${PR_TF}${PR_DO}${BLUE}%~${PR_USER_OP}${NO_COLOR} '
PS2=$'%_>'
RPROMPT=''

TIMER_THRESHOLD=30

# START OF cmdtime.plugin.zsh modified from https://github.com/tom-auger/cmdtime/blob/main/cmdtime.plugin.zsh
zmodload zsh/datetime

__cmdtime_current_time() {
  echo $EPOCHREALTIME
}

__cmdtime_format_duration() {
  local hours=$(printf '%u' $(($1 / 3600)))
  local mins=$(printf '%u' $((($1 - hours * 3600) / 60)))
  local secs=$(printf "%.3f" $(($1 - 60 * mins - 3600 * hours)))
  if [[ ! "${mins}" == "0" ]] || [[ ! "${hours}" == "0" ]]; then
      # If there are a non zero number of minutes or hours
      # then display integer number of seconds
      secs=${secs%\.*}
  elif [[ "${secs}" =~ "^0\..*" ]]; then
      # If secs starts with 0. i.e. is less than 1 then display
      # the number of milliseconds instead. Strip off the leading
      # zeros and append an 'm'.
      secs="${${${secs#0.}#0}#0}m"
  else
      # Display seconds to 2dp
      secs=${secs%?}
  fi
  local duration_str=$(echo "${hours}h:${mins}m:${secs}s")
  local format="${TIMER_FORMAT:-%d}"
  echo "${format//\%d/${${duration_str#0h:}#0m:}}"
}

__cmdtime_save_time_preexec() {
  __cmdtime_cmd_start_time=$(__cmdtime_current_time)
}

__cmdtime_display_cmdtime_precmd() {
  if [ -n "${__cmdtime_cmd_start_time}" ]; then
    local cmd_end_time=$(__cmdtime_current_time)
    local tdiff=$((cmd_end_time - __cmdtime_cmd_start_time))
    unset __cmdtime_cmd_start_time
    if [[ -z "${TIMER_THRESHOLD}" || ${tdiff} -ge "${TIMER_THRESHOLD}" ]]; then
        local tdiffstr=$(__cmdtime_format_duration ${tdiff})
        local cols=$((COLUMNS - ${#tdiffstr} - 1))
        echo -e "Execution time: ${tdiffstr}"
    fi
  fi
}

autoload -U add-zsh-hook
add-zsh-hook preexec __cmdtime_save_time_preexec
add-zsh-hook precmd __cmdtime_display_cmdtime_precmd
# END OF `cmdtime.plugin.zsh`


#------------------------------
# (Emacs) vterm
#------------------------------

if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
	source ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
fi

#------------------------------
# Dumb terminal
#------------------------------

# Overwrite fancy setting if the terminal is dumb.
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
