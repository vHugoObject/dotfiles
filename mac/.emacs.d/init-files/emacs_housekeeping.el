(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)  
(setopt warning-minimum-level :emergency
	inhibit-startup-message t
	create-lockfiles nil
	;; don't ask for confirmation when opening symlinked file
	vc-follow-symlinks t
	;; for tramp
	vc-handled-backends '(SVN Git)
	remote-file-name-inhibit-locks t)

(global-set-key (kbd "C-x C-b") 'buffer-menu-other-window)

(use-package cus-edit
  :after (use-package-ensure-system-package)
  :straight (:type built-in)
  :custom
  (custom-file null-device "Don't store customizations"))

(use-package frame
  :after (use-package-ensure-system-package)
  :straight (:type built-in)
  :custom
  (initial-frame-alist
       '((top . 1) (left . 1) (width . 200) (height . 60)))
 )

(use-package dired
  :after (use-package-ensure-system-package)
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
