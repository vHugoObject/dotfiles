#!/bin/zsh


create_symlinks() {
    
    # get directory containing symlinks
    # use command substitution with pwd so full path is returned
    # type -d: only look for directories
    # get files in .zsh folder
    
    script_dir=$(find $(pwd) -type d -name '.zsh')

    # use find to find all zsh dotfiles
    # . represents the current directory
    # set maxdepth 2 so we only look in folders in the path
    # use path to get all files in the .zsh path
    # as long as they are not named .DS_Store
    # use xargs to call basename on each filepath so we only get filenames
    
    for file in $(find . -maxdepth 2 -path './.zsh/*' \! -name '.DS_Store' | xargs -n 1 basename);do
	 echo "Creating symlink to $file in home directory."

         # create symlinks for each file in $HOME 
         # -s = symbolics links option
         # -f = option to replace any links unconditonally
         # ln [OPTION]... TARGET... DIRECTORY
         
         ln -fs $script_dir/$file ~/$file
      
    done
    
    # update the shell so that changes take effect immediately
    source ~/.zshrc
    
}

create_symlinks

