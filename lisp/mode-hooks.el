
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :bind
  (:map yas-minor-mode-map
        ("<tab>" . johmue/indent-and-maybe-yas-expand))
  ("C-<tab>" . johmue/yas-expand))

(use-package yasnippet-snippets)

(require 'ansi-color)

(defun johmue/colorize-compilation ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'johmue/colorize-compilation)

(use-package string-inflection
  :bind ("<f4>" . string-inflection-all-cycle))

(add-hook 'prog-mode-hook #'which-function-mode)
(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'prog-mode-hook (lambda ()
                            (setq fill-column 88)
                            (setq company-backends '(company-capf
                                                     (company-dabbrev-code company-keywords)
                                                     company-dabbrev))))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq company-backends '(company-capf))
                                  (setq indent-tabs-mode nil)
                                  (setq-local completion-at-point-functions
                                              (list (cape-super-capf #'elisp-completion-at-point #'cape-dabbrev)
                                                    #'cape-file))))

(dolist (mode '(text-mode-hook
                prog-mode-hook))
  (add-hook mode (lambda() (display-fill-column-indicator-mode t))))

(add-hook 'text-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (set-fill-column 79)
            (flyspell-mode 1)
            (setq indent-tabs-mode nil)
            (setq-local company-backends '(company-wordfreq))
            (setq-local company-transformers nil)))

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

(use-package py-isort
  :after python)

(require 'lsp-pylsp)
(with-eval-after-load "lsp-mode"
;;   (add-to-list 'lsp-disabled-clients 'pyls)
;;   (add-to-list 'lsp-enabled-clients 'pylsp)
  (add-to-list 'lsp-disabled-clients 'pyright)
;;   (add-to-list 'lsp-enabled-clients 'elixir-ls)
;;   (add-to-list 'lsp-enabled-clients 'rust-analyzer))
  )

(add-hook 'python-mode-hook
          (lambda ()
            (lsp)
            (require 'dap-python)
            (setq dap-python-debugger 'debugpy)))

(add-hook 'window-state-change-hook (lambda () (johmue/auto-activate-virtualenv)))

(require 'dap-python)

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

(defun dap-python-johmue-populate-test-at-point (conf)
  "Populate CONF with the required arguments."
  (if-let ((test (test-cockpit--python--test-function-path)))
      (plist-put conf :program test)
    (user-error "`dap-python': no test at point"))
  (plist-put conf :cwd (lsp-workspace-root))
  (dap-python--populate-start-file-args conf))

(dap-register-debug-provider "python-test-at-point-johmue" 'dap-python-johmue-populate-test-at-point)
(dap-register-debug-template "Python :: johmue Run pytest (at point)"
                             (list :type "python-test-at-point-johmue"
                                   :args ""
                                   :program nil
                                   :module "pytest"
                                   :request "launch"
                                   :name "Python :: johmue Run pytest (at point)"))

(setq-default lsp-pylsp-configuration-sources ["flake8"])
(setq-default lsp-pylsp-plugins-flake8-enabled t)

(use-package ein
  :hook (ein:ipynb-mode . (lambda () (johmue/auto-activate-virtualenv)))
  :config
  (setq ein:output-area-inlined-images t))

(use-package rustic
  :commands rustic-mode)
(add-hook 'rustic-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (lsp)
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
            (setq indent-tabs-mode t)
            (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
            (lsp)))

(use-package cask-mode)

(use-package sql-indent)

(add-hook 'sqlind-minor-mode-hook
          (lambda ()
            (setq sqlind-basic-offset 8)
            (add-to-list 'sqlind-indentation-offsets-alist '(defun-start 0))))

(use-package toml
  :straight (toml :type git :host github :repo "gongo/emacs-toml"
                  :fork (:host github
                               :repo "johannes-mueller/emacs-toml"
                               :branch "johmue-merges")))

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

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.leex\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.heex\\'" . web-mode))
  (setq web-mode-enable-element-content-fontification t)
  (setq web-mode-enable-element-tag-fontification t)
  (setq web-mode-enable-current-element-highlight t))

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
  (js2-mode . (lambda () (lsp-deferred))))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(use-package typescript-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  :hook
  (typescript-mode . (lambda () (lsp-deferred))))

(use-package ng2-mode)

(define-derived-mode ng2-web-mode
  web-mode "ng2-web"
  "Major mode for Angular 2 templates"
  (font-lock-add-keywords nil ng2-html-font-lock-keywords))

(add-to-list 'auto-mode-alist '("\\.component.html\\'" . ng2-web-mode))
(add-hook 'ng2-web-mode-hook (lambda () (lsp)))

(use-package lsp-sonarlint)
(push 'ng2-ts-mode (lsp--client-major-modes (gethash 'sonarlint lsp-clients)))
(push 'ng2-html-mode (lsp--client-major-modes (gethash 'sonarlint lsp-clients)))

(use-package json-mode
  :commands json-mode)

(use-package elixir-mode
  :hook (elixir-mode . (lambda ()
                         (add-to-list 'exec-path "/home/joh/.mix/elixir-ls/release")
                         (setq-local lsp-enable-file-watchers nil)
                         (lsp-deferred))))

(use-package alchemist)
(use-package elixir-yasnippets)
;(use-package flycheck-elixir)
;;; mode-hooks.el ends here
