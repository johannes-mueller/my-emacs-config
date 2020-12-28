(use-package projectile
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-mode-line-function '(lambda () (format " <%s>" (projectile-project-name))))
  (setq projectile-switch-project-action #'johmue/counsel-switch-to-project))

(use-package projectile-ripgrep)

(use-package selectrum
  :init
  (selectrum-mode 1))
(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode 1))

(use-package consult
  :bind (("C-c h" . consult-history)
	 ("C-c o" . consult-outline)
	 ("C-x b" . consult-buffer)
	 ("C-x 4 b" . consult-buffer-other-window)
	 ("C-x 5 b" . consult-buffer-other-frame)
	 ("C-x r x" . consult-register)
	 ("C-x r b" . consult-bookmark)
	 ("M-s o" . consult-outline)
	 ("M-s m" . consult-mark)
	 ("M-s l" . consult-line)
	 ("M-s i" . consult-imenu)
	 ("M-s e" . consult-error)
	 ("M-s m" . consult-multi-occur)
	 ("M-y" . consult-yank-pop)
	 ("<help> a" . consult-apropos))
  :init
  (fset 'multi-occur #'consult-multi-occur)
  :config
  (consult-preview-mode))

(use-package consult-selectrum
  :demand t)

(use-package marginalia
  :init
  (marginalia-mode))

(use-package ctrlf
  :init (ctrlf-mode 1))
