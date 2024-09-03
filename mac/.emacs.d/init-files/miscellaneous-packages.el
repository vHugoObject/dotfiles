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

(use-package emr
  :bind (:map prog-mode-map
       ("M-RET" . 'emr-show-refactor-menu))
  )
