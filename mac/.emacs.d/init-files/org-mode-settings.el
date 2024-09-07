(use-package org
  :after (use-package-ensure-system-package)
  :straight nil
  :mode ("\\.org\\'" . org-mode)
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  :hook ((org-trigger . save-buffer)
	 (org-mode . flyspell-mode)
	 (org-mode . verb-mode)
	 )
  :custom (org-todo-keywords
   '((sequence "TODO(t!)" "WAIT(w@/!)" "|" "DONE(d@!)" "CANCELED(c@)")))
  (org-treat-insert-todo-heading-as-state-change t "log TODO creation")
  (org-log-into-drawer "LOGBOOK" "log into LOGBOOK drawer")
  (org-log-done 'time)
  (org-startup-align-all-tables t)
  (org-startup-folded 'show2levels)
  )

(use-package org-auto-tangle
  :after (org)
  :hook (org-mode . org-auto-tangle-mode)
  )

(use-package org-table
  :after (org)
  :straight (:type built-in)
  :custom (org-table-duration-custom-format 'hh:mm "format for the output of calc computations")
 )

(use-package org-attach
  :after (org)
  :straight (:type built-in)
  :custom (org-attach-method 'l "set symbolic link as a default attachment method")
 )

(use-package org-clock
  :after (org)
  :straight (:type built-in)
  :custom (org-clock-clocked-in-display 'both  "display clock in both mode-line and frame-title")
	(org-clock-persist t "save the running clock when emacs is closed")
 )

(use-package org-duration
  :after (org)
  :straight (:type built-in)
  :custom (org-duration-format (quote h:mm) "Duration format will always be hours:minutes")
 )

(use-package org-habit
  :straight (:type built-in)
  :custom (add-to-list 'org-modules "org-habit" "add habits to org-modules")
 )

(defun my-org-confirm-babel-evaluate (lang body)
  "Custom function for org-confirm-babel. Contains of
   a set of functions that don't need confirmation
   for evaluation"
  (let ((langs (list "elisp" "emacs-lisp"
		     "zsh" "sh"
		     "shell" "gnuplot"
		     "sqlite" "sqlite3"
		     )))
    (not (member lang langs))      
    )
  )
(use-package ob-core
  :after (org)
  :straight (:type built-in)
  :config (org-babel-do-load-languages
	    'org-babel-load-languages (quote ((emacs-lisp . t)
					      (sqlite . t)
					      (shell . t))))
  :custom ((org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)	     	     
	   )

 )

(load "~/.emacs.d/init-files/org-capture-templates")
  (use-package org-capture
    :after (org)
    :straight (:type built-in)
    :custom (org-capture-templates org-capture-templates)
    )

(use-package org-pomodoro
  :after (org)
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
