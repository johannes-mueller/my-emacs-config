;;; Code;

(setenv "LANG" "en_US.UTF-8")

(setq load-path (add-to-list 'load-path "~/.emacs.d/lisp"))

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

(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode)
(blink-cursor-mode 0)

(setq visible-bell 1)

(setq indicate-buffer-boundaries 'right)

(use-package diminish
  :config
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :diminish
  :config
  (setq highlight-indent-guides-method 'bitmap)
  :hook (prog-mode . highlight-indent-guides-mode))

(column-number-mode 1)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		dired-mode-hook
                term-mode-hook
		vterm-mode
		shell-mode-hook
                eshell-mode-hook
		treemacs-mode-hook))
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
  :config (smartparens-global-mode t)
  :init
  (bind-key "M-<right>" 'sp-forward-sexp)
  (bind-key "M-<left>" 'sp-backward-sexp)
  (require 'smartparens-config)
)

(use-package which-key
  :init (which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 0.3))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-switch-project-action #'projectile-dired))

(use-package projectile-ripgrep)

(use-package prescient
  :config
  (prescient-persist-mode 1))

;(load "selectrum-consult")
(load "ivy-counsel")

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package undo-tree
  :diminish
  :init (global-undo-tree-mode))

(setq make-backup-files nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package flycheck
  :config (global-flycheck-mode))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package company
  :diminish
  :init (global-company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-indent-or-complete-common)
	      ("<return>" . nil)
	      ("RET" . nil)
	      ("<end>" . company-complete-selection)
	      ("C-<return>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  )

(use-package company-prescient
  :config
  (company-prescient-mode 1))

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
  (setq vterm-max-scrollback 10000))

(load "mode-hooks")
(load "johmue-defuns")
(load "key-bindings")
(load "look")
(load "spelling")

(load-theme 'johmue t)
