
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))
(use-package yasnippet-snippets)

(use-package string-inflection
  :bind ("<f4>" . string-inflection-all-cycle))

(add-hook 'prog-mode-hook #'which-function-mode)
(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'prog-mode-hook (lambda () (setq fill-column 88)))
(add-hook 'prog-mode-hook
	  (lambda()
	    (whitespace-mode t)
	    ))

(use-package whitespace
  :config
  (setq whitespace-line-column 127)
  (setq whitespace-style '(face trailing lines space-before-tab newline empty
				space-after-tab space-mark tab-mark newline-mark
				missing-newline-at-eof))
  (set-face-attribute 'whitespace-tab nil :background "red")
  )

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
	    (setq indent-tabs-mode nil)
	    (lsp)
	    (setq fill-column 100)
	    (setq-local whitespace-style (cons
					  (car whitespace-style)
					  (cons 'tabs (cdr whitespace-style))))
	    (whitespace-mode 1)
	    (require 'dap-gdb-lldb)))



;;; mode-hooks.el ends here
