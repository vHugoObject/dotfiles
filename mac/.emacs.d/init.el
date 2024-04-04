;; emacs housekeeping
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq warning-minimum-level :emergency)

 ; don't ask for confirmation when opening symlinked file
(setq vc-follow-symlinks t)     

(setq inhibit-startup-message t)
(setq dired-kill-when-opening-new-dired-buffer t)

(setq display-line-numbers t)
(setq create-lockfiles nil)

(defun goto-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

; fast quit
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

(require 'rust-mode)
