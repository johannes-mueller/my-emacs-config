

;;; Code;

(setq load-path (add-to-list 'load-path "~/.emacs.d/lisp"))
(load "proxy-settings" t)

(setq gc-cons-threshold 128000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq native-comp-async-report-warnings-errors nil)

(pgtk-use-im-context nil)

(require 'package)

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))

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

(defun johmue/popper-display-function (buffer &optional alist)
  (let ((window-settings
         (if (>= (frame-width) 270)
             `((window-width . ,(/ (frame-width) 3))
               (side . right)
               (slot . 1))
         `((window-height . ,(/ (frame-height) 3))
           (side . bottom)
           (slot . 1)))))
    (display-buffer-in-side-window
     buffer
     (append alist window-settings))))

(use-package popper
  :after projectile
  :ensure t
  :init
  (setq popper-group-function #'popper-group-by-projectile)
  (setq popper-display-function #'johmue/popper-display-function)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*xref\\*"
          "\\*Backtrace\\*"
          grep-mode
          help-mode
          compilation-mode
          helpful-mode))
  (popper-mode 1)
  (popper-echo-mode 1)
  (global-set-key [f8] 'popper-toggle)
  (global-set-key [M-f8] 'popper-cycle)
  )

(setq compilation-scroll-output t)

(setq split-width-threshold 210)
(setq split-height-threshold 150)

(use-package switchy-window
  :ensure t
  :custom (switchy-window-delay 1.0) ;; That's the default value.
  :bind
  (:map switchy-window-minor-mode-map
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
(require 'dired-x)

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
   ("C-c n" . #'mc/insert-numbers)
   ("C-c N" . #'johmue/mc/insert-numbers-1)
   ("C-c C-n" . #'johmue/mc/insert-numbers-prompt)
   ("C-c l" . #'mc/insert-letters)
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
  ("C-<return>" . crux-smart-open-line)
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
  (setq projectile-completion-system 'default)
  (setq projectile-keymap-prefix (kbd "C-c C-p"))
  (setq projectile-switch-project-action 'projectile-commander)
  (setq projectile-mode-line-function '(lambda () (format " <%s>" (projectile-project-name))))
  (setq projectile-project-root-functions '(projectile-root-local
                                            projectile-root-marked
                                            projectile-root-top-down
                                            projectile-root-top-down-recurring
                                            projectile-root-bottom-up))
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
  (magit-format-file-function #'magit-format-file-all-the-icons)
  :config
  (setq magit-git-environment (cons "LANG=en" magit-git-environment))
  :hook (git-commit-setup . (lambda () (adict-change-dictionary "english"))))

(use-package git-timemachine :after magit)

(use-package forge)
;; (use-package code-review
;;   :straight (code-review :type git :host github :repo "doomelpa/code-review"))

(use-package git-modes)

(use-package blamer
  :ensure t
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 40)
  (blamer-self-author-name "You")
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 110
                    :italic t)))
  :init
  (setq global-blamer-mode t))

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
              ("C-c a" . eglot-code-actions)
              ("C-c I" . eglot-inlay-hints-mode)
              ("C-c c" . johmue/eglot-string-inflection))
  :config
  (add-to-list 'eglot-server-programs `((elixir-ts-mode elixir-mode) . ("elixir-ls")))
  (add-to-list 'eglot-server-programs `((prisma-ts-mode) . ("prisma-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs `((python-ts-mode) . ("basedpyright-langserver" "--stdio")))
  (setq eglot-report-progress nil)
  (setq eglot-events-buffer-config '(:size 0 :format full)))

(use-package flycheck-eglot
  :init
  (global-flycheck-eglot-mode 1)
  (setq eldoc-display-functions '(eldoc-display-in-buffer))
)

(use-package eglot-booster
  :straight (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode))

(use-package editorconfig
  :ensure t
  :diminish
  :config
  (editorconfig-mode 1))

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

(defun -python-nav-beginning-of-statement ()
  "Move to start of current statement."
  (interactive "^")
  (forward-line 0)
  (let* ((ppss (syntax-ppss))
         (context-point
          (or
           (python-syntax-context 'paren ppss)
           (python-syntax-context 'string ppss))))
    (message "context-point %s %s %s %s" context-point (python-syntax-context 'paren ppss) (python-syntax-context 'string ppss) ppss)
    (cond ((bobp))
          (context-point
           (goto-char context-point)
           (python-nav-beginning-of-statement))
          ((save-excursion
             (forward-line -1)
             (python-info-line-ends-backslash-p))
           (forward-line -1)
           (python-nav-beginning-of-statement))))
  (message "now at %s" (point))
  (back-to-indentation)
  (point-marker))


(defun rst-python-statement-is-docstring (begin)
  "Return true if beginning of statiment is :begin"
  (save-excursion
    (save-match-data
      ;(message "point before %s %s %s" (point) (thing-at-point 'word 'no-properties) begin)
      (python-nav-beginning-of-statement)
      ;(message "point after %s %s" (point) (thing-at-point 'word 'no-properties) )
      (looking-at-p begin))))

(defun rst-python-front-verify ()
  (let ((result (rst-python-statement-is-docstring (match-string 0))))
    ;(message "match-string %s %s %s %s" (match-string 0) (point) (line-number-at-pos) result)
    result))


(use-package mmm-mode
  :config
  (setq mmm-global-mode 'maybe)
  (mmm-add-classes
   '((python-rst
      :submode rst-mode
      :face mmm-comment-submode-face
      :front "r?\\(\"\"\"\\|'''\\)"
      :front-verify (lambda () t)
      :back "\\(\"\"\"\\|'''\\)"
      :end-not-begin t
      :save-matches 1
      ;; :front rst-python-docstrings-find-front
      ;; :back rst-python-docstrings-find-back
      :insert ((?d embdocstring nil @ "r\"\"\"" @ _ @ "\"\"\"" @))
      :delimiter-mode nil)))
  (mmm-add-mode-ext-class 'python-ts-mode nil 'python-rst)
                                        ;(add-hook 'mmm-python-rst-hook )
  )

;(add-to-list 'mmm-save-local-variables 'completion-at-point-functions)

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

(use-package auth-source-kwallet
  :config
  (setq auth-source-kwallet-executable "kwallet-query-wrapper.sh")
  (setq auth-source-kwallet-wallet "kdewallet")
  (auth-source-kwallet-enable))

(use-package tramp
  :config
  (setq tramp-default-method "ssh"))

(use-package docker
  :ensure t
  :bind ("C-x D" . docker))

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

(use-package devcontainer-mode
  :straight (devcontainer-mode :type git :host github :repo "johannes-mueller/devcontainer-mode"))


(put 'package-lint-main-file 'safe-local-variable #'stringp)
