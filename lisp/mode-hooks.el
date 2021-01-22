
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map
	("<tab>" . johmue/indent-and-maybe-yas-expand))
  ("C-<tab>" . yas-expand))

(use-package yasnippet-snippets)


(use-package string-inflection
  :bind ("<f4>" . string-inflection-all-cycle))

(add-hook 'prog-mode-hook #'which-function-mode)
(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'prog-mode-hook (lambda ()
			    (setq fill-column 88)
		            (setq company-backends '(company-bbdb company-semantic company-capf company-clang company-files
					(company-dabbrev-code company-gtags company-etags company-keywords)
					company-oddmuse company-dabbrev))
			    (company-fuzzy-mode 1)))

(dolist (mode '(text-mode-hook
		prog-mode-hook))
  (add-hook mode (lambda() (display-fill-column-indicator-mode t))))

(add-hook 'text-mode-hook
	  (lambda ()
	    (turn-on-auto-fill)
	    (set-fill-column 79)
	    (flyspell-mode 1)
	    (setq company-backends '(company-dabbrev company-ispell))
	    (company-fuzzy-mode 0)
	    ))

(use-package wc-mode
  :hook (text-mode . (lambda () (wc-mode 1))))

(use-package visual-fill-column :diminish)

(use-package pyvenv)
(use-package python
  :bind (:map inferior-python-mode-map
	      ("C-r" . comint-history-isearch-backward))
  )

(use-package py-autopep8
  :custom
  (py-autopep8-options '('("--max-line-length=100"))))

(use-package py-isort)

(add-hook 'python-mode-hook
	  (lambda ()
	    (johmue/auto-activate-virtualenv)
	    (lsp)
	    (require 'dap-python)
	    (setq dap-python-debugger 'debugpy)
))
(setq-default lsp-pyls-configuration-sources ["flake8"])
(setq lsp-pyls-plugins-flake8-enabled t)

(use-package ein
  :hook (ein:ipynb-mode . (lambda ()
			    (johmue/auto-activate-virtualenv))))

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


(use-package dtrt-indent)

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (c-toggle-auto-hungry-state nil)
	    (c-toggle-auto-newline nil)
	    (dtrt-indent-mode t)
	    (setq-default c-basic-offset 8 c-default-style "linux")
	    (setq-default c-indentation-style "linux")
	    (setq-default tab-width 8 indent-tabs-mode t)
	    (setq indent-tabs-mode t)
	    (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
	    (lsp)))

(use-package cask-mode)

(use-package sql-indent)

(add-hook 'sqlind-minor-mode-hook
	  (lambda ()
	    (setq sqlind-basic-offset 8)
	    (add-to-list 'sqlind-indentation-offsets-alist '(defun-start 0))))
;;; mode-hooks.el ends here
