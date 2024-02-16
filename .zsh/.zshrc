#!/bin/zsh

# emacs keybindings
bindkey -e

# Save command history
HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history
HISTSIZE=2000
SAVEHIST=1000


# link configs files
[ -e "${HOME}/.zsh_aliases" ] && source "${HOME}/.zsh_aliases"
[ -e "${HOME}/.zsh_functions" ] && source "${HOME}/.zsh_functions"
[ -e "${HOME}/.zsh_options" ] && source "${HOME}/.zsh_options"
