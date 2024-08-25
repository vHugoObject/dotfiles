(setopt user-emacs-directory "~/.emacs.d/")

(setopt init-files-directory "init-files")
(setopt package-manager-file "package-management")
(load (file-name-concat user-emacs-directory init-files-directory package-manager-file))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)  
(setopt warning-minimum-level :emergency)
(setopt inhibit-startup-message t)

    (setopt create-lockfiles nil)
    ;; don't ask for confirmation when opening symlinked file
    (setopt vc-follow-symlinks t)
    ;; for tramp
    (setopt vc-handled-backends '(SVN Git))
    (setopt remote-file-name-inhibit-locks t)

(global-set-key (kbd "C-x C-b") 'buffer-menu-other-window)

(defun my-kill-emacs ()
"save some buffers, then exit unconditionally"
(interactive)
(save-some-buffers nil t)
(kill-emacs))
;; create a keymap with new function
(global-set-key (kbd "C-x C-c") 'my-kill-emacs)

(use-package cus-edit
  :straight (:type built-in)
  :custom
  (custom-file null-device "Don't store customizations"))

(use-package frame
  :straight (:type built-in)
  :custom
  (initial-frame-alist
       '((top . 1) (left . 1) (width . 200) (height . 60)))
 )

(use-package dired
  :straight (:type built-in)
  :hook (dired-mode . (lambda ()
	    (define-key dired-mode-map
	      (kbd "C-c C-x a")
	      #'org-attach-dired-to-subtree)))
  :custom ((dired-recursive-deletes t)
	   (dired-vc-rename-file t)
	   (dired-create-destination-dirs 'ask)
	   )	     
 )

(cl-flet (
	(memberp (file-list file-name)
	  (member file-name file-list)
	  )
	)    
(let* (
       (init-files-folder (file-name-concat user-emacs-directory init-files-directory))
       (ignore '("package-management.el"))
       (ignore-list (mapcar (lambda (file)
			      (file-name-concat user-emacs-directory init-files-directory file)
			      )
			    ignore)
	)
       (file-regex "\.el$")
       (first-list (directory-files init-files-folder t file-regex))
       (second-list
	(seq-remove (apply-partially #'memberp ignore-list) first-list))	 
       (file-list (mapcar (lambda (file)
			     (file-name-sans-extension file)
			     )
			   second-list)
		   )
   )
    (mapc (lambda (file) (load file))	     
	 file-list))
)
