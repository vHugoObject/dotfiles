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

(eval-when-compile
    (require 'use-package))
(use-package use-package-ensure-system-package :ensure t)

(use-package package
  :custom
  (package-enable-at-startup nil)
	  (package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))  				     )

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
     )

;; only load rust-mode when needed
(use-package rust-mode
  :mode "\\.rs\\'"
)

(use-package js-mode
  :mode ( "\\.js\\'" "\\.mjs\\'")
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

(use-package frame
  :custom
  (initial-frame-alist
       '((top . 1) (left . 1) (width . 200) (height . 60)))
 )

(use-package spray
  :bind ("C-<f6>" . spray-mode)
  :mode ("\\.epub\\'" "\\.txt\\'")
  :custom ((spray-margin-left 80)
	   (spray-margin-top 5)
	   (set-frame-font "Iosevka Extended 12" nil t)
	   )
  )

;; autosave on TODO state change
(use-package org
  :hook ((org-trigger . save-buffer)
	 (org-mode . flyspell-mode)
	 )
  :custom
  (org-todo-keywords
   '((sequence "TODO(t!)" "WAIT(w@/!)" "|" "DONE(d@!)" "CANCELED(c@)")))
  (org-treat-insert-todo-heading-as-state-change t "log TODO creation")
  (org-log-into-drawer "LOGBOOK" "log into LOGBOOK drawer")
  (org-log-done 'time)
  (org-startup-align-all-tables t)
  (org-startup-folded 'show2levels)
  )

(use-package org-table
:custom (org-table-duration-custom-format 'hh:mm "format for the output of calc computations")
 )

(use-package org-attach
:custom (org-attach-method 'l "set symbolic link as a default attachment method")
 )

(use-package org-clock
:custom (org-clock-clocked-in-display 'both  "display clock in both mode-line and frame-title")
	(org-clock-persist t "save the running clock when emacs is closed")
 )

(use-package org-duration
:custom (org-duration-format (quote h:mm) "Duration format will always be hours:minutes")
 )

(use-package org-habit
:custom (add-to-list 'org-modules "org-habit" "add habits to org-modules")
	(org-habit-preceding-days )
	(org-habit-)
	(org-habit-graph-column)
 )

(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "emacs-lisp"))) ;don't ask for emacs-lisp
(use-package ob-core
:custom (org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)
 )

(use-package org-capture
  :custom (org-capture-templates
	   '(("c" "Les calories" table-line (file+headline "~/org/la-nourriture.org" "Les calories")
	      "|%T|%^{PROMPT|500}|")
	     ("s" "La sucre" table-line (file+headline "~/org/la-nourriture.org" "La sucre")
	      "|%T|%^{PROMPT}|")
	     ("a" "Les dépenses" table-line (file+headline "~/org/les-sous.org" "Les dépenses")
		"|%T|%^{PROMPT}|")
	     ))
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

(use-package dired
  :hook (dired-mode . (lambda ()
	    (define-key dired-mode-map
	      (kbd "C-c C-x a")
	      #'org-attach-dired-to-subtree)))
  :custom ((dired-recursive-deletes t)
	   (dired-vc-rename-file t)
	   (dired-create-destination-dirs 'ask)
	   )	     
 )

(use-package tramp
  :custom (add-to-list 'tramp-remote-path 'tramp-own-path)
	  (add-to-list 'tramp-connection-properties
	   (list (regexp-quote "/ghcs")
		 "remote-shell" "/usr/bin/zsh"))
	  (customize-set-variable 'tramp-encoding-shell "/usr/bin/zsh")

  )

(use-package magit
  :ensure t
  :bind (("C-c C-g" . magit-status)
        ("C-c p" . magit-push-to-remote)
	 )
 )

(use-package codespaces
  :ensure-system-package gh
  :config (codespaces-setup)
  :bind ("C-c S" . #'codespaces-connect)
  )

(use-package cus-edit
  :custom
  (custom-file null-device "Don't store customizations"))

(add-to-list 'load-path "~/.emacs.d/custom-functions")

(use-package org-table-custom-functions)
(use-package org-table-custom-functions-tests)
