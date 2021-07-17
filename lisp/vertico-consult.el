(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-c C-p"))
  (setq projectile-globally-ignored-files '("TAGS" "#*"))
  :config
  (projectile-global-mode)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package consult
  :bind (("C-S-s" . consult-ripgrep)
	 ("C-M-s" . consult-line)
	 ("C-o" . consult-outline)
	 ("C-x b" . consult-buffer)
	 ("M-y" . consult-yank-pop)
	 ("C-c i" . consult-imenu)
	 :map minibuffer-local-map
	 ("C-r" . consult-history))

  :config
  (setq consult-root-function 'projectile-project-root)
  (setq consult-line-start-from-top nil))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings)))

(setq embark-action-indicator
      (lambda (map _target)
        (which-key--show-keymap "Embark" map nil nil 'no-paging)
        #'which-key--hide-popup-ignore-command)
      embark-become-indicator embark-action-indicator)

(use-package embark-consult)

(use-package orderless
   :ensure t
   :custom (completion-styles '(orderless)))

(defvar johmue/line-search-command 'consult-line)
