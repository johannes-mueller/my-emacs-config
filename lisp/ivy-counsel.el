
(use-package counsel
  :diminish
  :bind (("C-x b" . 'counsel-switch-buffer)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode)
  (setq counsel-projectile-preview-buffers t)
  (setq projectile-mode-line-function '(lambda () (format " <%s>" (projectile-project-name))))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-command-map (kbd ".") #'projectile-run-vterm))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper-isearch)
	 ("C-S-s" . swiper-isearch-thing-at-point)
	 ("C-r" . swiper-isearch-backward)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq projectile-completion-system 'ivy)
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1))


(use-package lsp-ivy)
