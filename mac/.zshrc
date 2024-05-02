#!/bin/zsh

# emacs keybindings
bindkey -e

# Save command history
HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history
HISTSIZE=5000
SAVEHIST=5000
HISTORY_IGNORE="(l|l *|ls|ls *|cd|cd ..*|cd -|z *|pwd|exit|caf|cddot)"

# link configs files if they exist
[[ -e "${HOME}/.zsh_functions" ]] && source "${HOME}/.zsh_functions"
[[ -e "${HOME}/.zsh_aliases" ]] && source "${HOME}/.zsh_aliases"
[[ -e "${HOME}/.zsh_options" ]] && source "${HOME}/.zsh_options"
[[ -e "${HOME}/.zsh_prompt" ]] && source "${HOME}/.zsh_prompt"
[[ -e "${HOME}/setting.sh" ]] && source "${HOME}/setting.sh"

