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




;; directory load custom packages from
;;(add-to-list 'load-path "~/.emacs.d/custom-packages")

;; only load rust-mode when needed
(use-package rust-mode
  :mode "\\.rs\\'"
)

(use-package js-mode
  :mode ( "\\.js\\'" "\\.mjs\\'" "\\.jsx\\'")
)


;; bind spray mode f6
(use-package spray
  :bind ("C-<f6>" . spray-mode))

;; org-mode settings
;; autosave on TODO state chan ge
(use-package org
  :hook (org-trigger . save-buffer)
  :custom
  (org-todo-keywords
   '((sequence "TODO(t!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
  (org-treat-insert-todo-heading-as-state-change t "log TODO creation")
  (org-log-into-drawer "LOGBOOK" "log into LOGBOOK drawer")
  (add-to-list 'org-modules "org-habit" "add habits to org-modules")
  )
  


;; org-pomodoro
(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :bind ("M-C-o" . org-pomodoro)
  ;; autosave on pomodorro finish
  :hook ((org-pomodoro-finished . save-buffer)
         (org-pomodoro-started . save-buffer)
	 (org-pomodoro-break-finished . save-buffer)
	 )
  :custom
   (org-pomodoro-length 20)
   (org-pomodoro-short-break-length 5)
   (org-pomodoro-clock-break t)
   (org-pomodoro-long-break-length 15)
   (org-pomodoro-manual-break t)
   )

(use-package magit
  :ensure t
  :bind (("C-x C-g" . magit-status)
        ("C-c p" . magit-push-to-remote)
	 )
 )

(use-package cus-edit
  :custom
  (custom-file null-device "Don't store customizations"))
