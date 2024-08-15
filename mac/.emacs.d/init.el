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
  :straight nil
  :custom
  (custom-file null-device "Don't store customizations"))

(use-package frame
  :straight nil
  :custom
  (initial-frame-alist
       '((top . 1) (left . 1) (width . 200) (height . 60)))
 )

(use-package dired
  :straight nil
  :hook (dired-mode . (lambda ()
	    (define-key dired-mode-map
	      (kbd "C-c C-x a")
	      #'org-attach-dired-to-subtree)))
  :custom ((dired-recursive-deletes t)
	   (dired-vc-rename-file t)
	   (dired-create-destination-dirs 'ask)
	   )	     
 )

(use-package lsp-mode
    :hook ((typescript-mode . lsp-deferred)
	   (rust-mode . lsp-deferred)
	   )
    :commands (lsp lsp-deferred)
    :custom (lsp-enable-snippet nil)
    )

(use-package rust-mode
  :mode "\\.rs\\'"
)

(use-package web-mode

  :mode ("\\.html?\\'" "\\.tsx\\'" "\\.jsx\\'")
  :custom (web-mode-enable current-element-highlight t)
)

(use-package typescript-mode
:mode "\\.ts\\'")

(use-package flycheck
:hook (typescript-mode js-mode web-mode-enable)
:custom (flycheck-add-mode 'javascript-eslint 'web-mode)
)

(use-package spray

  :bind ("C-<f6>" . spray-mode)
  :mode ("\\.epub\\'" "\\.txt\\'")
  :custom ((spray-margin-left 80)
	   (spray-margin-top 5)
	   (set-frame-font "Iosevka Extended 12" nil t)
	   )
  )

(use-package magit
  :bind (("C-c C-g" . magit-status)
	("C-c p" . magit-push-to-remote)
	 )
 )

(use-package codespaces
  :ensure-system-package gh
  :config (codespaces-setup)
  :bind ("C-c S" . #'codespaces-connect)
  :straight (codespaces.el :type git :host github :repo "patrickt/codespaces.el"
                    :fork t)
  )

(use-package verb
  :straight (verb :type git :host github :repo "federicotdn/verb"
		    :fork t)
  )

(load "~/.emacs.d/init-files/org-mode-settings")

(load "~/.emacs.d/init-files/org-table-custom-functions")
(load "~/.emacs.d/init-files/org-table-custom-functions-tests")
