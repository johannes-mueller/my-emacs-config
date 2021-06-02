;;; Code;

(setq load-path (add-to-list 'load-path "~/.emacs.d/lisp"))

(load "proxy-settings" t)

(setq gc-cons-threshold 128000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

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

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

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

(use-package default-text-scale
  :config
  (default-text-scale-mode 1))

(use-package popwin
  :config
  (push '(compilation-mode :noselect t :height 0.3 :position bottom :tail t) popwin:special-display-config)
  (push '("*Help*" :height 0.5 :position top) popwin:special-display-config)
  (push '("*xref*" :height 0.3 :position bottom) popwin:special-display-config)
  (push '("*grep*" :height 0.5 :position bottom) popwin:special-display-config)
  (push '(python-pytest-mode :height 0.5 :position bottom) popwin:special-display-config)
  (push '(magit-mode :position right) popwin:special-display-config)
  (popwin-mode 1)
)

(setq compilation-scroll-output t)

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
		helpful-mode-hook
		compilation-mode-hook))
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

(use-package imenu-list
  :bind (("C-'" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize nil))

(use-package avy
  :bind
  ("C-:" . avy-goto-char-timer)
  ("C-'" . avy-goto-char-2)
  (:map isearch-mode-map
	("C-M-s" . avy-isearch)))


;(load "selectrum-consult")
(load "ivy-counsel")

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-traditional)
  (magit-diff-refine-hunk (quote all))
  :config
  (setq magit-git-environment (cons "LANG=en" magit-git-environment)))

(use-package gitignore-mode :after magit)
(use-package gitattributes-mode :after magit)
(use-package gitconfig-mode :after magit)
(use-package git-timemachine :after magit)

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
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.venv.*\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.venv\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\_venv.*\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\__pycache__\\'")
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-M-l")
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-enable nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-imenu-enable t)
  )

(use-package lsp-treemacs
  :after lsp)

(use-package dap-mode
  :commands (dap-debug dap-mode)
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
	      ("C-<return>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.2)
  (company-require-match nil)
  :config
  (setq company-ispell-dictionary (file-truename "~/.emacs.d/dicts/en.txt"))
  )

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1)
  (setq company-prescient-sort-length-enable t))

(use-package company-fuzzy
  :after company
  :config
  (setq company-fuzzy-prefix-on-top t)
  ;(setq company-fuzzy-sorting-function 'johmue/company-fuzzy-no-dabbrev)
)
(straight-use-package
 '(company-wordfreq :type git :host github :repo "johannes-mueller/company-wordfreq.el"))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "zsh")
  (setq term-prompt-regexp "^[a-z]*@[a-z]* [^<]* <[0-9]*> %")
  (term-set-escape-char ?\C-x)
  )

;(use-package eterm-256color
;  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :bind (:map vterm-mode-map
	      ("C-<right>" . vterm--self-insert)
	      ("C-<left>" . vterm--self-insert)
	      ("M-<right>" . vterm--self-insert)
	      ("M-<left>" . vterm--self-insert)
	      ("M-<up>" . vterm--self-insert)
	      ("C-r" . vterm--self-insert)
	      ("C-g" . vterm--self-insert)
	      ("C-u" . vterm--self-insert)
	      ("C-y" . vterm--self-insert))
  :config
  (setq term-prompt-regexp "^[a-z]*@[a-z]* [^<]* <[0-9]*> %")
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000)
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  )

(use-package ssh-agency)

(use-package tramp
  :config
  (setq tramp-default-method "ssh"))

(load "mode-hooks")
(load "johmue-defuns")
(load "key-bindings")
(load "look")
(load "spelling")

(use-package autothemer)
(load-theme 'johmue t)

(load "local-devel" t)
