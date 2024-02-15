#!/bin/zsh

#emacs keybindings
bindkey -e

# Save command history
HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history
HISTSIZE=2000
SAVEHIST=1000


#auto/tab complete
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.


#set options
setopt AUTO_CD
setopt NO_SHORT_LOOPS
setopt COMPLETE_IN_WORD
setopt NO_CLOBBER
setopt ALWAYS_TO_END
setopt AUTO_LIST
setopt INTERACTIVE_COMMENTS
setopt HIST_IGNORE_SPACE
setopt HIST_SAVE_NO_DUPS
setopt SHARE_HISTORY
setopt HIST_REDUCE_BLANKS
setopt INC_APPEND_HISTORY

# link configs files
[ -e "${HOME}/.zsh_aliases" ] && source "${HOME}/.zsh_aliases"
[ -e "${HOME}/.zsh_functions" ] && source "${HOME}/.zsh_functions"
[ -e "${HOME}/.zsh_exports" ] && source "${HOME}/.zsh_exports" 



