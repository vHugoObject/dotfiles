#!/bin/zsh

# emacs keybindings
bindkey -e

# Save command history
HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history
HISTSIZE=5000
SAVEHIST=1000

# turn functions path into a set
typeset -U fpath
fpath=(~/zfunc $fpath)                        
#autoload functions
for func in $^fpath/*(N-.:t);do autoload -Uz $func;done


# link configs files if they exist
[[ -e "${HOME}/.zsh_functions" ]] && source "${HOME}/.zsh_functions"
[[ -e "${HOME}/.zsh_aliases" ]] && source "${HOME}/.zsh_aliases"
[[ -e "${HOME}/.zsh_options" ]] && source "${HOME}/.zsh_options"
[[ -e "${HOME}/.zsh_prompt" ]] && source "${HOME}/.zsh_prompt"

