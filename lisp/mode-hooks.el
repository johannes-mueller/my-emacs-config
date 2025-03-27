
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map
        ("<tab>" . johmue/yas-expand)))

(use-package yasnippet-snippets)

(use-package yasnippet-capf)

(require 'ansi-color)


(use-package compile
  :ensure nil
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (ansi-color-for-compilation-mode t)
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))


(use-package string-inflection
  :bind
  ("<f4>" . johmue/string-inflection-cycle-auto)
  ("S-<f4>" . string-inflection-all-cycle))

(defun johmue/eglot-string-inflection ()
  (interactive)
  (eglot-rename
   (string-inflection-python-style-cycle-function (symbol-name (symbol-at-point)))))

(defun johmue/string-inflection-cycle-auto ()
  "switching by major-mode"
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   (t
    (string-inflection-python-style-cycle))))


(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq indent-tabs-mode nil)
                                  (setq-local completion-at-point-functions
                                              (list (cape-capf-super #'elisp-completion-at-point #'cape-dabbrev #'yasnippet-capf)
                                                    'cape-file))
                                  (setq-local lisp-indent-function #'Fuco1/lisp-indent-function)))

(dolist (mode '(text-mode-hook
                prog-mode-hook))
  (add-hook mode (lambda() (display-fill-column-indicator-mode t))))

(add-hook 'mmm-python-ts-mode-rst-mode-hook #'johmue/enter-text-submode)
(add-hook 'mmm-rst-mode-python-ts-mode-hook #'johmue/exit-text-submode)

(setq johmue/completion-at-point-functions '())

(defun johmue/enter-text-submode ()
  (setq johmue/completion-at-point-functions completion-at-point-functions)
  (johmue/text-mode-hook))

(defun johmue/exit-text-submode ()
  (johmue/prog-mode-hook)
  (when johmue/completion-at-point-functions
    (setq completion-at-point-functions johmue/completion-at-point-functions)))

(defun johmue/prog-mode-hook ()
  (flycheck-posframe-mode)
  (which-function-mode)
  (auto-fill-mode -1)
  (show-paren-mode)
  (setq fill-column 88)
  (setq-local completion-ignore-case nil)
  (setq-local corfu-auto-delay 0.2))

(defun johmue/text-mode-hook ()
  (message "prog-mode=hook %s" (bound-and-true-p yas-minor-mode))
  (turn-on-auto-fill)
  (set-fill-column 79)
  (setq indent-tabs-mode nil)
  (setq-local completion-at-point-functions '(capf-wordfreq-completion-at-point-function))
  (setq-local corfu-sort-function 'identity)
  (setq-local corfu-auto-delay 0.8))

(add-hook 'prog-mode-hook #'johmue/prog-mode-hook)
(add-hook 'text-mode-hook #'johmue/text-mode-hook)

(use-package wc-mode
  :hook (text-mode . (lambda () (wc-mode 1))))

(use-package visual-fill-column
  :diminish
  :custom
  (visual-fill-column-with 79))

(use-package pandoc
  :after pandoc-mode)
(use-package pandoc-mode
  :hook (markdown-mode . (lambda () (pandoc-mode 1))))

(use-package yaml-mode
  :hook (yaml-mode . (lambda()
                       (auto-fill-mode -1))))
(use-package yaml-tomato)

(use-package pyvenv
  :after python-mode)

(use-package uv
  :straight (uv :type git :host github :repo "johannes-mueller/uv.el"))

(use-package cython-mode
  :straight (cython-mode :type git :host github :repo "johannes-mueller/emacs-cython-mode" :branch "updates"))

(use-package py-isort
  :after python)

(use-package python-black
  :config
  (setq python-black-command "~/.pyenvs/pylsp/bin/black")
  (setq python-black-macchiato-command "~/.pyenvs/pylsp/bin/black-macchiato")
  (setq python-black-extra-args '("--skip-string-normalization")))

(add-hook 'python-mode-hook (lambda () (setq electric-indent-inhibit nil)))
(add-hook 'cython-mode-hook (lambda () (setq electric-indent-inhibit t)))

(defun johmue/python-black-format-defun ()
    (interactive)
  (save-excursion
    (beginning-of-defun)
    (let ((beg (point)))
      (end-of-defun)
      (python-black-region beg (point) nil))))

(use-package elixir-ts-mode
  :hook
  (elixir-ts-mode . (lambda () (eglot-ensure))))

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (rust-mode . rust-ts-mode)
        (json-mode . json-ts-mode)
        (elixir-mode . elixir-ts-mode)
        ))


(setq-default eglot-workspace-configuration
              '(
                :pylsp  (:configurationSources
                           ["flake8"]
                           :plugins (
                                     :pycodestyle (:enabled :json-false)
                                     :mccabe (:enabled t)
                                     :pyflakes (:enabled :json-false)
                                     :flake8 (:enabled t :maxLineLength 88)
                                     :ruff (:enabled t :lineLength 88)
                                     :pydocstyle (:enabled t :convention "numpy")
                                     :yapf (:enabled :json-false)
                                     :autopep8 (:enabled :json-false)
                                     :black (:enabled t
                                                      :line_length 88
                                                      :cache_config t)
                                     :jedi_completion (:enabled t
                                                                :include_params t
                                                                :eager t
                                                                :include_class_objects t
                                                                :fuzzy t)
                                     ;:rope_autoimport (:enabled t)
                                     )
                         )
                :rust-analyzer (:editor (:formatOnType (:enebled :json-false)))
               )
              )


(add-hook 'python-ts-mode-hook (lambda () (message "python-ts-mode %s" (bound-and-true-p yas-minor-mode))))
(add-hook 'python-ts-mode-hook (lambda ()
                                 (eglot-ensure)
                                 (setq-local completion-at-point-functions
                                             (list (cape-capf-super #'eglot-completion-at-point #'yasnippet-capf)
                                                    #'cape-file
                                                    #'python-completion-at-point))))

(add-hook 'window-selection-change-functions #'johmue/auto-activate-virtualenv)

(use-package ein
  :hook (ein:ipynb-mode . (lambda () (johmue/auto-activate-virtualenv)))
  :config
  (setq ein:output-area-inlined-images t))

(use-package rustic
  :commands rustic-mode
  :config
  (setq rustic-lsp-client 'eglot))

(add-hook 'rustic-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (eglot-ensure)
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
            (setq-local indent-tabs-mode t)
            (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
            (eglot-ensure)))

(use-package cask-mode)

(use-package sql-indent)

(add-hook 'sqlind-minor-mode-hook
          (lambda ()
            (setq sqlind-basic-offset 8)
            (add-to-list 'sqlind-indentation-offsets-alist '(defun-start 0))))

(use-package toml)

(use-package test-cockpit-python
  :straight (test-cockpit :type git :host github :repo "johannes-mueller/test-cockpit.el"))

(use-package test-cockpit-cask
  :straight (test-cockpit :type git :host github :repo "johannes-mueller/test-cockpit.el"))

(use-package test-cockpit-cargo
  :straight (test-cockpit :type git :host github :repo "johannes-mueller/test-cockpit.el"))

(use-package test-cockpit-mix
  :straight (test-cockpit :type git :host github :repo "johannes-mueller/test-cockpit.el"))

(use-package test-cockpit-npm-jest
  :straight (test-cockpit :type git :host github :repo "johannes-mueller/test-cockpit.el"))

(test-cockpit-add-custom-action 'python-toml "D" "build docs" "sphinx-build -b html docs build/html")
(test-cockpit-add-dynamic-custom-action 'python-toml "t" "mypy this file" "mypy %f")

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.leex\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.heex\\'" . web-mode))
  (setq web-mode-enable-element-content-fontification t)
  (setq web-mode-enable-element-tag-fontification t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-indentation nil))

;; use smart parens in web mode
(defun my-web-mode-hook ()
  (setq web-mode-enable-auto-pairing nil))

(add-hook 'web-mode-hook  'my-web-mode-hook)

(defun sp-web-mode-is-code-context (id action context)
  (and (eq action 'insert)
       (not (or (get-text-property (point) 'part-side)
                (get-text-property (point) 'block-side)))))

(sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

(use-package js2-mode
  :commands js2-mode
  :hook
  (js2-mode . (lambda () (eglot-ensure))))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(use-package typescript-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  :hook
  (typescript-mode . (lambda () (eglog-ensure))))

(use-package ng2-mode)

(define-derived-mode ng2-web-mode
  web-mode "ng2-web"
  "Major mode for Angular 2 templates"
  (font-lock-add-keywords nil ng2-html-font-lock-keywords))

(add-to-list 'auto-mode-alist '("\\.component.html\\'" . ng2-web-mode))
(add-hook 'ng2-web-mode-hook (lambda () (eglot-ensure)))

(use-package json-ts-mode
  :commands json-ts-mode)

(use-package dockerfile-ts-mode
  :straight (:type built-in)
  :defer t
  :mode (("\\Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.dockerignore\\'" . dockerfile-ts-mode)))


(use-package toml-ts-mode
  :straight (:type built-in)
  :mode "\\.toml\\'"
  :defer t)


(use-package yaml-ts-mode
  :straight (:type built-in)
  :mode "\\.ya?ml\\'")

;(use-package flycheck-elixir)


(use-package dape
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  (setq dape-key-prefix "\C-x\C-d")

  :hook
  (dape-repl-mode . (lambda () (corfu-mode -1) (message "corfu mode disabled")))
  ;; Save breakpoints on quit
  ;; ((kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  ;;  (after-init . dape-breakpoint-load))

  :init
  ;; To use window configuration like gud (gdb-mi)
  (setq dape-buffer-window-arrangement 'right)

  :config
  ;; Info buffers to the right
  ;; (setq dape-buffer-window-arrangement 'right)

  ;; Global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)

  ;; To not display info and/or buffers on startup
  ;(remove-hook 'dape-on-start-hooks 'dape-info)
  ;(remove-hook 'dape-on-start-hooks 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-on-stopped-hooks 'dape-info)
  ;; (add-hook 'dape-on-stopped-hooks 'dape-repl)

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-compile-hooks 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))

  ;; Projectile users
  (setq dape-cwd-fn 'projectile-project-root)
  )

(use-package prisma-ts-mode
  :ensure t
  :straight (prisma-ts-mode :type git
                            :host github
                            :repo "johannes-mueller/prisma-ts-mode"
                            :branch "johmue-merges")

  :config
  (setq prisma-ts-mode-indent-level 4)

  :hook
  (prisma-ts-mode . (lambda ()
                      (setq-local tab-width 4)
                      (setq-local indent-tabs-mode nil)
                      (eglot-ensure)))

  :bind
  (:map prisma-ts-mode-map
        ("M-q" . prisma-format-declaration)
        ("C-M-q" . eglot-format)))

(add-to-list 'treesit-language-source-alist '(prisma "https://github.com/victorhqc/tree-sitter-prisma"))
(add-to-list 'treesit-language-source-alist '(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile"))

;; From MIT licensed https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L12-L94
(defun Fuco1/lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
      ;; car of form doesn't seem to be a symbol, or is a keyword
      ((and (elt state 2)
            (or (not (looking-at "\\sw\\|\\s_"))
                (looking-at ":")))
       (if (not (> (save-excursion (forward-line 1) (point))
                   calculate-lisp-indent-last-sexp))
           (progn (goto-char calculate-lisp-indent-last-sexp)
                  (beginning-of-line)
                  (parse-partial-sexp (point)
                                      calculate-lisp-indent-last-sexp 0 t)))
       ;; Indent under the list or under the first sexp on the same
       ;; line as calculate-lisp-indent-last-sexp.  Note that first
       ;; thing on that line has to be complete sexp since we are
       ;; inside the innermost containing sexp.
       (backward-prefix-chars)
       (current-column))
      ((and (save-excursion
              (goto-char indent-point)
              (skip-syntax-forward " ")
              (not (looking-at ":")))
            (save-excursion
              (goto-char orig-point)
              (looking-at ":")))
       (save-excursion
         (goto-char (+ 2 (elt state 1)))
         (current-column)))
      (t
       (let ((function (buffer-substring (point)
                                         (progn (forward-sexp 1) (point))))
             method)
         (setq method (or (function-get (intern-soft function)
                                        'lisp-indent-function)
                          (get (intern-soft function) 'lisp-indent-hook)))
         (cond ((or (eq method 'defun)
                    (and (null method)
                         (> (length function) 3)
                         (string-match "\\`def" function)))
                (lisp-indent-defform state indent-point))
               ((integerp method)
                (lisp-indent-specform method state
                                      indent-point normal-indent))
               (method
                (funcall method indent-point state))))))))

(setq lisp-indent-function #'Fuco1/Lisp-Indent-Function)

;;; mode-hooks.el ends here
