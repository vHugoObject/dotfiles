(add-to-list 'load-path "~/.emacs.d/init-files")

(load "~/.emacs.d/init-files/package-management")

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
       (file-path "~/.emacs.d/init-files")
       (ignore '("~/.emacs.d/init-files/package-management"))
       (file-regex "\.org$")
       (first-list (directory-files file-path nil file-regex))
       (second-list (mapcar (lambda (item)
			    (concat file-path "/" (string-remove-suffix ".org" item))
			    )
			  first-list))
       (file-list
	(seq-remove (apply-partially #'memberp ignore) second-list))
		   )
   (mapc (lambda (file) (load file))	     
	 file-list)
   )
  )
