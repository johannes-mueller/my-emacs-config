;;; Code;

(setenv "LANG" "en_US.UTF-8")

(setq load-path (add-to-list 'load-path "~/.emacs.d/lisp"))

(load "proxy-settings" t)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq-default custom-file null-device)

(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 0)

(global-hl-line-mode)

(setq visible-bell 1)

(setq indicate-buffer-boundaries 'right)

(use-package diminish
  :config
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'whitespace-mode)
)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (
	   (doom-modeline-height 20)
	   (doom-modeline-project-detection 'projectile)
	   (doom-modeline-major-mode-icon t)
	   (doom-modeline-major-mode-color-icon t)
	   (doom-modeline-major-mode-color-icon t)
	   (doom-modeline-buffer-state-icon t)
	   (doom-modeline-minor-modes t)
	   )
  )

(use-package auto-dim-other-buffers
  :config
  (auto-dim-other-buffers-mode t))

(use-package popwin
  :config
  (push '(compilation-mode :noselect t :height 0.3 :position bottom :tail t) popwin:special-display-config)
  (push '("*Help*" :height 0.5 :position top) popwin:special-display-config)
  (push '("*xref*" :height 0.3 :position bottom) popwin:special-display-config)
  (push '("*grep*" :height 0.5 :position bottom) popwin:special-display-config)
  (push '(python-pytest-mode :height 0.5 :position bottom) popwin:special-display-config)
  (popwin-mode 1)
)

(setq split-width-threshold 210)
(setq split-height-threshold 150)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :diminish
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-enabled nil)
  :hook (prog-mode . highlight-indent-guides-mode))

(column-number-mode 1)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		dired-mode-hook
		term-mode-hook
		vterm-mode-hook
		shell-mode-hook
		eshell-mode-hook
		treemacs-mode-hook
		helpful-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq dired-listing-switches "-agho --group-directories-first")


(require 'scroll-in-place)

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)
	 ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package smartparens
  :diminish
  :config
  (sp-pair "(" ")" :unless '(sp-point-before-word-p))
  (sp-pair "[" "]" :unless '(sp-point-before-word-p))
  (sp-pair "{" "}" :unless '(sp-point-before-word-p))
  (smartparens-global-mode t)
  :init
  (bind-key "M-<right>" 'sp-forward-sexp)
  (bind-key "M-<left>" 'sp-backward-sexp)
  (require 'smartparens-config)
)

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package move-text
  :bind (("C-M-<up>" . move-text-up)
	 ("C-M-<down>" . move-text-down)))

(use-package crux
  :bind
  ("C-k" . crux-smart-kill-line)
  ("C-S-<backspace>" . crux-kill-whole-line)
  ("C-S-<return>" . crux-smart-open-line-above)
  ("S-<return>" . crux-smart-open-line)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c w" . crux-rename-file-and-buffer)
  ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
  ([remap move-beginning-of-line] . crux-move-beginning-of-line)
  )

(use-package which-key
  :init (which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 0.3))

(use-package guru-mode
  :config
  (setq guru-warn-only t)
  (guru-global-mode +1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package prescient
  :config
  (prescient-persist-mode 1))

(use-package avy
  :bind
  ("C-:" . avy-goto-char-timer)
  ("C-'" . avy-goto-char-2)
  )

;(load "selectrum-consult")
(load "ivy-counsel")

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


(use-package gitignore-mode)
(use-package gitattributes-mode)
(use-package gitconfig-mode)

(use-package forge
  :after magit
  )

(use-package diff-hl
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
	 (magit-post-refresh . diff-hl-magit-post-refresh))
  :config (global-diff-hl-mode))

(use-package undo-tree
  :diminish
  :init (global-undo-tree-mode))

(setq make-backup-files nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package flycheck
  :config (global-flycheck-mode))

(defun johmue/org-mode-hook ()
  (org-indent-mode)
  )

(use-package org
  :hook ((org-mode . johmue/org-mode-hook))
  :custom
  (org-agenda-files '("~/OrgFiles/"))
  (org-ellipsis " ▾")
  )

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
)

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-M-l")
  :config
  (setq gc-cons-threshold 128000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-peek-enable t)
  (lsp-ui-imenu-enable t)
  :bind
  ([remap xref-find-definitions] . lsp-find-definitions)
  ([remap xref-find-references] . lsp-find-references)
  )

(use-package lsp-treemacs
  :after lsp)

(use-package dap-mode
  :config
  (require 'dap-node)
  (dap-node-setup))

(use-package company
  :diminish
  :init (global-company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-complete)
	      ("<return>" . nil)
	      ("RET" . nil)
	      ("<end>" . company-complete-selection)
	      ("C-<return>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  )

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1)
  (setq company-prescient-sort-length-enable t))

;(use-package company-box
;  :hook (company-mode . company-box-mode))

(use-package term
  :config
  (setq explicit-shell-file-name "zsh")
  (setq term-prompt-regexp "^[a-z]*@[a-z]* [^<]* <[0-9]*> %")
  (term-set-escape-char ?\C-x)
  )

;(use-package eterm-256color
;  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[a-z]*@[a-z]* [^<]* <[0-9]*> %")
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000)
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
)

(load "mode-hooks")
(load "johmue-defuns")
(load "key-bindings")
(load "look")
(load "spelling")

(use-package autothemer)
(load-theme 'johmue t)

(load "local-devel" t)
