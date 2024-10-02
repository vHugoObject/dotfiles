(setopt user-emacs-directory "~/.emacs.d/"
	init-files-directory "init-files"
	personal-packages "personal-packages"
	)

(setopt straight-fix-flycheck t)
(setopt straight-use-package-by-default t)
(setopt straight-repository-branch "master")
(setopt straight-host-usernames '((github . "vHugoObject")))
(setopt straight-default-vc 'git)

(defvar bootstrap-version)
(let ((bootstrap-file
	 (expand-file-name
	  "straight/repos/straight.el/bootstrap.el"
	  (or (bound-and-true-p straight-base-dir)
	      user-emacs-directory)))
	(bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
	  (url-retrieve-synchronously
	   "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	   'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

(use-package use-package-ensure-system-package
  :init (straight-use-package 'use-package)
  )

(let* ((file-regex "\.el$")
       (init-files-folder (file-name-concat user-emacs-directory init-files-directory))	 
       (first-list (directory-files init-files-folder 't file-regex))
       (init-files (mapcar #'file-name-sans-extension first-list))	 
       (personal-packages-directory (file-name-concat user-emacs-directory personal-packages))
       (second-list (directory-files-recursively personal-packages-directory file-regex))
       (personal-packages-files (mapcar #'file-name-sans-extension second-list))
       (file-list (seq-concatenate 'list init-files personal-packages-files)))    
    (mapc #'load file-list))
