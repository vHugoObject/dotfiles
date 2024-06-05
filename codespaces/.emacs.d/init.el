;; remove menu bar and toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; force buffer menu to always open in other window
(global-set-key (kbd "C-x C-b") 'buffer-menu-other-window)

(setq warning-minimum-level :emergency)
(setq inhibit-startup-message t)
(setq dired-kill-when-opening-new-dired-buffer t)
(setq create-lockfiles nil)
;; don't ask for confirmation when opening symlinked file
(setq vc-follow-symlinks t)     


(defun goto-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

;; fast quit
(defun my-kill-emacs ()
  "save some buffers, then exit unconditionally"
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))
(global-set-key (kbd "C-x C-c") 'my-kill-emacs)

;; setup use-package
(eval-when-compile
  (require 'use-package))

;; package management
(use-package package
  :custom
  (package-enable-at-startup nil)
	  (package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))  				     )



;; only load rust-mode when needed
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
)

(use-package js-mode
  :ensure t
  :mode ( "\\.js\\'" "\\.mjs\\'")
)

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" "\\.tsx\\'" "\\.jsx\\'")
  :custom (web-mode-enable current-element-highlight t)
)


(use-package typescript-mode
:ensure t
:mode "\\.ts\\'")

(use-package flycheck
:hook (typescript-mode js-mode web-mode-enable)
:custom (flycheck-add-mode 'javascript-eslint 'web-mode)
)

(use-package frame
  :custom
  (initial-frame-alist
       '((top . 1) (left . 1) (width . 200) (height . 60)))
 )

(use-package magit
  :ensure t
  :bind (("C-c C-g" . magit-status)
        ("C-c p" . magit-push-to-remote)
	 )
 )

(use-package cus-edit
  :custom
  (custom-file null-device "Don't store customizations"))
