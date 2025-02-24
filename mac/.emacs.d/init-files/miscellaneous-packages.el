(use-package spray
  :after (use-package-ensure-system-package)
  :straight (spray :type git :host github :repo "emacsmirror/spray")
  :bind ("C-<f6>" . spray-mode)
  :mode ("\\.epub\\'" "\\.txt\\'")
  :custom ((spray-margin-left 80)
	   (spray-margin-top 5)
	   (set-frame-font "Iosevka Extended 12" nil t)
	   )
  )

(use-package magit
  :after (use-package-ensure-system-package)
  :bind (("C-c C-g" . magit-status)
	("C-c p" . magit-push-to-remote)
	 )
 )

(use-package codespaces
  :after (use-package-ensure-system-package)
  :ensure-system-package gh
  :config (codespaces-setup)
  :bind ("C-c S" . #'codespaces-connect)
  :straight (codespaces.el :type git :host github :repo "patrickt/codespaces.el"
		    :fork t)
  )

(use-package emr
  :after (use-package-ensure-system-package)
  :bind (:map prog-mode-map
       ("M-RET" . 'emr-show-refactor-menu))
  )
