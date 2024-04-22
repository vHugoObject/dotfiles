;; emacs housekeeping
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq warning-minimum-level :emergency)

;; don't ask for confirmation when opening symlinked file
(setq vc-follow-symlinks t)     

(setq inhibit-startup-message t)
(setq dired-kill-when-opening-new-dired-buffer t)


(setq display-line-numbers t)
(setq create-lockfiles nil)

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


;; path management
(use-package exec-path-from-shell
  :ensure t)
;; necessary to get packages to install on mac
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))



;; package management
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))

;; directory load custom packages from
(add-to-list 'load-path '~/.emacs.d/custom-packages')

;; packages
(require 'rust-mode)
(require 'spray)

;; shortcut for spray reader
(global-set-key (kbd "<f6>") 'spray-mode)

;; org-mode settings

;; add habits to org-modules
(add-to-list 'org-modules 'org-habit t)

;; define global TODO keywords
;; ! for a timestamp
;; @ for a note with a timestamp
(setq org-todo-keywords
           '((sequence "TODO(t!)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

			
;; log TODO creation
(setq org-treat-insert-todo-heading-as-state-change t)

;; log into LOGBOOK drawer
(setq org-log-into-drawer "LOGBOOK")

