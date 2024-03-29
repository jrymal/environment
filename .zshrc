#!/usr/bin/env zsh
export DOTFILES=$HOME/environment
export INCLUDES=~/.local/shell

source $DOTFILES/startTmux

source $DOTFILES/env
source $DOTFILES/dircolors
source $DOTFILES/aliases

if [[ -d "$INCLUDES" && 
    -z "$(find $INCLUDES -maxdepth 0 -type d -empty 2> /dev/null)" ]]; then
    source  $INCLUDES/*
else
    echo "No directory or files for local setup found at $INCLUDES"
fi


HISTFILE=$HOME/.zsh_history

zstyle ':completion:*' menu select
zstyle ':completion:*' completer _complete
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '+l:|=* r:|=*'

autoload -U compinit && compinit
zmodload -i zsh/complist

unsetopt menu_complete
unsetopt flowcontrol

setopt chase_dots
setopt chase_links
setopt prompt_subst
setopt always_to_end
setopt append_history
setopt auto_menu
setopt complete_in_word
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt interactivecomments
setopt share_history
setopt autocd autopushd pushdignoredups

alias -s {properties,patch,css,source,txt,orig,diff,retry,new,log,java,js,conf,c,cpp,h,hpp,xslt,xml,yml,yaml}=$EDITOR
alias -s {de,net,com,org,htm,html}=$BROWSER
alias -s tar.gz=tar xzvf
alias -s gz=gunzip
alias -s zip=unzip

bindkey -v
bindkey '^a' beginning-of-line
bindkey '^e' end-of-line
bindkey '^R' history-incremental-search-backward

git_prompt() {
  local BRANCH=$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/*\(.*\)/\1/')

  if [ ! -z $BRANCH ]; then
    echo -n "%F{yellow}$BRANCH"

    if [ ! -z "$(git status --short)" ]; then
      echo " %F{red}✗"
    fi
  fi
}

restart_prompt() {
  if [ -e /var/run/reboot-required ]; then 
    local RESTART=$(cat /var/run/reboot-required)
    if [ ! -z $RESTART ]; then
        echo "%F{yellow}${RESTART} ";
    fi
  fi
}

vim_prompt() {
  if [ ! -z $VIMRUNTIME ]; then
    echo ":%F{green}sh ";
  fi
}

TRAPZERR() {
  local LAST_COMMAND=$_ 
  local LAST_EXIT_CODE=$?
  if [[ $LAST_EXIT_CODE -ne 0 ]]; then
    echo "${COLOR_RED}ERROR: ${COLOR_YELLOW}${LAST_COMMAND}" 
  fi
}

command_not_found_handler() {
  local LAST_COMMAND=$1 
  local LAST_ARGS=$2
  echo "${COLOR_RED}command not found: ${COLOR_YELLOW}${LAST_COMMAND}" 
}

PS1='
%F{yellow}[%D{%Y-%m-%f}T%D{%H:%M:%S}]
$(restart_prompt)$(vim_prompt)%F{blue}%~$(git_prompt)
%F{244}%# %F{reset}'

