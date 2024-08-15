(use-package org
  :straight nil
  :hook ((org-trigger . save-buffer)
	 (org-mode . flyspell-mode)
	 )
  ;; for verb    
  :custom (org-todo-keywords
   '((sequence "TODO(t!)" "WAIT(w@/!)" "|" "DONE(d@!)" "CANCELED(c@)")))
  (org-treat-insert-todo-heading-as-state-change t "log TODO creation")
  (org-log-into-drawer "LOGBOOK" "log into LOGBOOK drawer")
  (org-log-done 'time)
  (org-startup-align-all-tables t)
  (org-startup-folded 'show2levels)
  )

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode)
  )

(use-package org-table
  :straight nil
  :custom (org-table-duration-custom-format 'hh:mm "format for the output of calc computations")
 )

(use-package org-attach
  :straight nil
  :custom (org-attach-method 'l "set symbolic link as a default attachment method")
 )

(use-package org-clock
  :straight nil
  :custom (org-clock-clocked-in-display 'both  "display clock in both mode-line and frame-title")
	(org-clock-persist t "save the running clock when emacs is closed")
 )

(use-package org-duration
  :straight nil
  :custom (org-duration-format (quote h:mm) "Duration format will always be hours:minutes")
 )

(use-package org-habit
  :straight nil
  :custom (add-to-list 'org-modules "org-habit" "add habits to org-modules")
	(org-habit-preceding-days )
	(org-habit-)
	(org-habit-graph-column)
 )

(defun my-org-confirm-babel-evaluate (lang body)
  "Custom function for org-confirm babel. Contains of
   a set of functions that don't need confirmation
   for evaluation"
  (let ((langs (list "elisp" "emacs-lisp")))
    (not (member lang langs))      
    )
  )

(use-package ob-core
  :straight nil
  :custom (org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)
 )

(use-package org-capture-templates
  :straight nil)

(use-package org-capture
  :straight nil
  :custom (org-capture-templates)
  )

(use-package org-pomodoro
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
