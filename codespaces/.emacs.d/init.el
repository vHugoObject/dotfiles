(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(line-number-mode 1)
(column-number-mode 1)  
(setopt warning-minimum-level :emergency
	inhibit-startup-message t
	create-lockfiles nil
	;; don't ask for confirmation when opening symlinked file
	vc-follow-symlinks t
	;; for tramp
	vc-handled-backends '(SVN Git)
	remote-file-name-inhibit-locks t)

(global-set-key (kbd "C-x C-b") 'buffer-menu-other-window)

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



;; setup use-package
(eval-when-compile
  (require 'use-package))

;; package management
(use-package package
  :custom
  (package-enable-at-startup nil)
	  (package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))  				     )


(use-package magit
  :ensure t
  :bind (("C-c C-g" . magit-status)
        ("C-c p" . magit-push-to-remote)
	 )
 )

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package rust-ts-mode
  :defer t
  :mode "\\.rs\\'")


(use-package js-ts-mode
  :after (use-package-ensure-system-package)
  :mode "\\.js\\'")

(use-package typescript-ts-mode
  :after (use-package-ensure-system-package)
  :defer t
  :mode "\\.tsx?\\'")

(use-package json-ts-mode
  :after (use-package-ensure-system-package)
  :defer t
  :mode "\\.json\\'")

(use-package eglot
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

(use-package flycheck
  :hook (after-init-hook . global-flycheck-mode)
  )
