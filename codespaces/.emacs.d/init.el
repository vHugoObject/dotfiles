;; emacs housekeeping
(menu-bar-mode -1)
(tool-bar-mode -1)

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

(use-package exec-path-from-shell
  :ensure t)
;; necessary to get packages to install on mac
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(use-package use-package-ensure-system-package :ensure t)


;; package management
(use-package package
  :custom package-enable-at-startup nil
	  (package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))  				     )


;; only load rust-mode when needed
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
)

(use-package js-mode
  :mode ( "\\.js\\'" "\\.mjs\\'" "\\.jsx\\'")
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
