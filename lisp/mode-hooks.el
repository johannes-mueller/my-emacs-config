
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




;;; mode-hooks.el ends here
