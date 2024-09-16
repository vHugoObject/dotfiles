(use-package treesit-auto
  :after (use-package-ensure-system-package)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package rust-ts-mode
  :after (use-package-ensure-system-package)
  :straight (:type built-in)
  :defer t
  :mode "\\.rs\\'")

(use-package js-ts-mode
  :after (use-package-ensure-system-package)
  :straight (:type built-in)
  :mode "\\.js\\'")

(use-package typescript-ts-mode
  :after (use-package-ensure-system-package)
  :straight (:type built-in)
  :defer t
  :mode "\\.tsx?\\'")

(use-package json-ts-mode
  :after (use-package-ensure-system-package)
  :straight (:type built-in)
  :defer t
  :mode "\\.json\\'")

(use-package flycheck
  :after (use-package-ensure-system-package)
  :hook (after-init-hook . global-flycheck-mode)
  )

(use-package eglot
  :after (use-package-ensure-system-package)
  :straight (:type built-in)
  :hook ((rust-mode . eglot-ensure)
	 (typescript-mode . lsp-deferred)
	 )
  :bind (:map eglot-mode-map
	    ("C-c d" . eldoc)
	    ("C-c a" . eglot-code-actions)
	    ("C-c f" . flymake-show-buffer-diagnostics)
	    ("C-c r" . eglot-rename))
  :config (add-to-list 'eglot-server-programs
		       '(((js-mode :language-id javascript)
					(js-ts-mode :language-id javascript)
					(tsx-ts-mode :language-id typescriptreact)
					(typescript-mode :language-id typescript))
				       . ("typescript-language-server" "--stdio"))
		       `(rust-mode . ("rust-analyzer" :initializationOptions
				     ( :procMacro (:enable t)
				       :cargo ( :buildScripts (:enable t)
						:features "all")))))
  )

(use-package package-lint
  :after (use-package-ensure-system-package)
  :straight (package-lint :type git :host github :repo "purcell/package-lint"
			  :fork t)
  )

(use-package compat
  :after (use-package-ensure-system-package)
  )
