#!/bin/zsh


create_symlinks() {
    
    # get directory containing symlinks
    # use command substitution with pwd so full path is returned
    # type -d: only look for directories
    # get files in .zsh folder
    
    script_dir=$(find $(pwd) -type d -name $1)

    # use find to find all zsh dotfiles
    # . represents the current directory
    # set maxdepth 2 so we only look in folders in the path
    # use path to get all files in the .zsh path
    # as long as they are not named .DS_Store
    # use xargs to call basename on each filepath so we only get filenames
    # use xargs -I to create a variable argument
    # {} is the argument list marker
    # -I replace occurrences of the replace-str, i.e. {}, with the names read into xargs
    
    for file in $(find . -maxdepth 2 -path "./${1}/*" \! -name '.DS_Store' | xargs -n 1 basename);do
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

install() {

    # The folder name with the dotfiles to be installed is based on whether the codespaces env variable exist
    # -n = does the variable that follows expand to a string of non-zero length
    
    if [[ -n "${CODESPACES}" ]]; then
	create_symlinks "codespaces"
    else
	create_symlinks "mac"
    fi
    
    
}

install


