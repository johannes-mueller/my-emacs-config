

;;; Code;

(setq load-path (add-to-list 'load-path "~/.emacs.d/lisp"))
(setenv "LSP_USE_PLISTS" "true")
(load "proxy-settings" t)

(setq gc-cons-threshold 128000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq native-comp-async-report-warnings-errors nil)

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

(setq make-backup-files nil)
(setq create-lockfiles nil)
(add-to-list 'completion-ignored-extensions "#")

(setq indicate-buffer-boundaries 'right)

(savehist-mode 1)
(recentf-mode 1)
(save-place-mode 1)
(global-auto-revert-mode)

(use-package diminish
  :config
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'whitespace-mode)
  (diminish 'eldoc-box-hover-at-point-mode)
  (diminish 'goggles-mode)
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
  (push '("*Package-Lint*" :height 0.5 :position bottom) popwin:special-display-config)
  (push '(python-pytest-mode :height 0.5 :position bottom) popwin:special-display-config)
  (push '(magit-mode :position right) popwin:special-display-config)
  (popwin-mode 1)
)

(setq compilation-scroll-output t)

(setq split-width-threshold 210)
(setq split-height-threshold 150)

(use-package switchy-window
  :ensure t
  :custom (switchy-window-delay 1.0) ;; That's the default value.
  :bind     :bind (:map switchy-window-minor-mode-map
                        ([f2] . switchy-window))
  :init
  (switchy-window-minor-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package highlight-indent-guides
  :diminish
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-auto-enabled nil)
  :hook (prog-mode . highlight-indent-guides-mode))

(require 'display-line-numbers)
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

(require 'dired)
(use-package dired-single)

(setq dired-listing-switches "-agho --group-directories-first")
(setq dired-kill-when-opening-new-dired-buffer t)

(use-package dired-gitignore
  :straight (dired-gitignore :type git
                             :host github
                             :repo "johannes-mueller/dired-gitignore.el"))

(dired-gitignore-global-mode)

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(require 'scroll-in-place)

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this-symbol)
   ("C-<" . mc/mark-previous-like-this-symbol)
   ("C-M->" . mc/mark-next-like-this-word)
   ("C-M-<" . mc/mark-previous-like-this-word)
   ("C-c C-<" . mc/mark-all-like-this-symbol)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)
   :map mc/keymap
   ("C-s" . johmue/isearch)
   ("C-r" . johmue/isearch-backward)))

(use-package phi-search)

;(setq case-replace nil)
(setq isearch-allow-motion t)
(setq isearch-lazy-count t)
(setq isearch-yank-on-move t)

(use-package smartparens
  :diminish
  :config
  (sp-pair "(" ")" :unless '(sp-point-before-word-p))
  (sp-pair "[" "]" :unless '(sp-point-before-word-p))
  (sp-pair "{" "}" :unless '(sp-point-before-word-p))
  (smartparens-global-mode t)
  :bind
  ("M-<right>" . 'sp-forward-sexp)
  ("M-<left>" . 'sp-backward-sexp)
  ("M-<backspace>" . 'sp-backward-kill-sexp)
  ("M-<delete>" . 'sp-kill-sexp)
  :init
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

(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-c C-p"))
  (setq projectile-switch-project-action 'projectile-commander)
  (setq projectile-mode-line-function '(lambda () (format " <%s>" (projectile-project-name))))
  :config
  (projectile-global-mode)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-command-map (kbd ".") #'projectile-run-vterm)
  (add-to-list 'projectile-globally-ignored-directories ".venv"))


;(load "selectrum-consult")
;(load "ivy-counsel")
(load "vertico-consult")


(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk (quote all))
  :config
  (setq magit-git-environment (cons "LANG=en" magit-git-environment))
  :hook (git-commit-setup . (lambda () (adict-change-dictionary "english"))))

(use-package git-timemachine :after magit)

(use-package forge)

(use-package git-modes)

(use-package blamer
  :ensure t
  :bind (("s-i" . blamer-show-commit-info)
         ("C-c i" . blamer-show-posframe-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  (blamer-self-author-name "You")
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 110
                    :italic t)))
  :init
  (setq global-blamer-mode nil))

(use-package diff-hl
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config (global-diff-hl-mode))

(use-package undo-tree
  :diminish
  :init
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package flycheck
  :config (global-flycheck-mode))

(use-package flycheck-posframe
  :init
  (setq flycheck-posframe-position 'window-bottom-right-corner)
  (setq flycheck-posframe-border-width 1)
  (flycheck-posframe-mode))

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("C-c a" . eglot-code-actions)))

(defun johmue/org-mode-hook ()
  (org-indent-mode)
  (define-key org-mode-map (kbd "<S-left>") 'johmue/jump-whitespace-backward)
  (define-key org-mode-map (kbd "<S-right>") 'johmue/jump-whitespace-forward)
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


(defun rst-python-statement-is-docstring (begin)
  "Return true if beginning of statiment is :begin"
  (save-excursion
    (save-match-data
      (python-nav-beginning-of-statement)
      (looking-at-p begin))))

(defun rst-python-front-verify ()
  (rst-python-statement-is-docstring (match-string 0)))


(use-package mmm-mode
  :config
  (setq mmm-global-mode 'maybe)
  (mmm-add-classes
   '((python-rst
      :submode rst-mode
      :face mmm-comment-submode-face
      :front "[ru]?\\(\"\"\"\\|\'\'\'\\)"
      :front-verify rst-python-front-verify
      :back "~1"
      :end-not-begin t
      :save-matches 1
      ;; :front rst-python-docstrings-find-front
      ;; :back rst-python-docstrings-find-back
      :insert ((?d embdocstring nil @ "u\"\"\"" @ _ @ "\"\"\"" @))
      :delimiter-mode nil)))
  (mmm-add-mode-ext-class 'python-mode nil 'python-rst)
                                        ;(add-hook 'mmm-python-rst-hook )
  )

;(add-to-list 'mmm-save-local-variables 'completion-at-point-functions)

(defun efs/lsp-mode-setup ()
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.venv.*\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\_venv.*\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\_*build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\__pycache__\\'")
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(setq lsp-use-plists t)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (lsp-mode . efs/lsp-mode-setup)
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  :init
  (setq lsp-keymap-prefix "C-M-l")
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-completion-provider :none)
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq lsp-log-max nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-enable nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-imenu-enable t)
  (lsp-signature-auto-activate nil)
  )

(use-package lsp-treemacs
  :after lsp)

(use-package dap-mode
  :commands (dap-debug dap-mode)
  :config
)

(use-package flycheck-eglot
  :init
  (global-flycheck-eglot-mode 1))


(defun johmue/eldoc-box--upper-right-window-corner-position-function (width _)
  "The default function to set childframe position.
Used by `eldoc-box-position-function'.
Position is calculated base on WIDTH and HEIGHT of childframe text window"
  (pcase-let ((`(,offset-l ,offset-r ,offset-t) eldoc-box-offset))
    (cons
     (- (+ (window-pixel-left (selected-window))
           (window-pixel-width (selected-window)))
        width offset-r)
     offset-t)))

(use-package eldoc-box
  :init
  (setq eldoc-box-position-function #'johmue/eldoc-box--upper-right-window-corner-position-function)
  (setq eldoc-box-offset '(32 48 16)))

(global-eldoc-mode nil)

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

(load "use-corfu")
(load "mode-hooks")
(load "johmue-defuns")
(load "key-bindings")
(load "look")
(load "spelling")

(use-package autothemer)
(load-theme 'johmue t)

(load "local-devel" t)

(when (file-exists-p (concat (file-name-as-directory "~/.emacs.d/lisp") "local-lisp.el"))
  (load "local-lisp"))
