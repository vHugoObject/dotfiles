(setopt user-emacs-directory "~/.emacs.d/"
	init-files-directory "init-files"
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

(let* (
       (init-files-folder (file-name-concat user-emacs-directory init-files-directory))	 
       (file-regex "\.el$")
       (first-list (directory-files init-files-folder t file-regex))	 
       (file-list (mapcar (lambda (file)
			     (file-name-sans-extension file)
			     )
			   first-list)
		   )
   )
    (mapc (lambda (file) (load file))	     
	 file-list))
