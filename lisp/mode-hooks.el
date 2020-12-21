
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))
(use-package yasnippet-snippets)

(add-hook 'prog-mode-hook #'which-function-mode)
(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'prog-mode-hook (lambda () (setq fill-column 88)))

(dolist (mode '(text-mode-hook
		prog-mode-hook))
  (add-hook mode (lambda() (display-fill-column-indicator-mode t))))

(add-hook 'text-mode-hook
	  (lambda ()
	    (turn-on-auto-fill)
            (setq fill-column 79)
	    (flyspell-mode 1)
	    (message "Text mode initiated")
	    ))

(use-package pyvenv)
(add-hook 'python-mode-hook
	  (lambda ()
	    (johmue/auto-activate-virtualenv)
	    (lsp)
	    (require 'dap-python)
	    (setq dap-python-debugger 'debugpy)
))
(setq-default lsp-pyls-configuration-sources ["flake8"])

(use-package rustic)
(add-hook 'rustic-mode-hook
	  (lambda ()
	    (cargo-minor-mode)
	    (setq indent-tabs-mode nil)
	    ))



;;; mode-hooks.el ends here
