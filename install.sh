#!/bin/zsh


create_symlinks() {
    
    # get directory containing symlinks
    # use command substitution with pwd so full path is returned
    # type -d: only look for directories
    # at depth one
    
    script_dir=$(find $(pwd) -type d -depth 1 -name '.zsh')

    # use find to find all zsh dotfiles
    # . represents the current directory
    # at depth 2 so we only look in folders in the path
    # all files with names that start with z
    # use xargs to call basename on each filepath so we only get filenames
    
    for file in $(find . -depth 2 -name '.zsh*' | xargs -n 1 basename);do
	 echo "Creating symlink to $file in home directory."

         # remove any of the current dotfiles from $HOME
         # r option - remove directories and their contents recursively
         # f option - ignore nonexistent files and arguments, never prompt
         # rm -rf ~/$file

         # create symlinks for each file in $HOME 
         # -s = symbolics links option
         # -f = option to replace any links unconditonally
         # ln [option]… -t directory target…
         ln -fs $script_dir/$file ~/$file
      
    done
    
    # update the shell so that changes take effect immediately
    source ~/.zshrc
    
}

create_symlinks

