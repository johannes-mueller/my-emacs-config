;; -*- lexical-binding: t; -*-

(use-package vertico
  :ensure t
  :init (vertico-mode)
  :bind (:map minibuffer-local-map
              ("<backspace>" . johmue/consult-find-file-backward-kill)
	      ("S-<down>" . forward-paragraph)
	      ("S-<up>" . backward-paragraph)
              ("RET" . +vertico-exit-minibuffer)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(defun +vertico-restrict-to-matches ()
  (interactive)
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert " ")
    (add-text-properties (minibuffer-prompt-end) (point-max)
                         '(invisible t read-only t cursor-intangible t rear-nonsticky t))))

(defun +vertico-exit-minibuffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (vertico-exit)))

(define-key vertico-map (kbd "S-SPC") #'+vertico-restrict-to-matches)

(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
(keymap-global-set "M-r" #'vertico-repeat)

(use-package marginalia
  :after vertico
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :bind (("C-S-s" . johmue/isearch-line-symbol-at-point)
	 ("C-S-r" . johmue/ripgrep-symbol-at-point)
	 ("C-M-S-r" . johmue/ripgrep-thing-at-point)
	 ("C-s-s" . consult-isearch-history)
	 ("<f3>" . consult-isearch-history)
	 ("s-s" . consult-line)
	 ("s-r" . consult-ripgrep)
	 ("s-g" . consult-grep)
	 ("C-c m" . consult-mark)
	 ("C-o" . consult-outline)
	 ("C-x b" . consult-buffer)
	 ("C-x C-b" . consult-project-buffer)
	 ("M-y" . consult-yank-pop)
	 ("C-c i" . consult-imenu)
	 :map minibuffer-local-map
	 ("C-r" . consult-history))

  :config
  (setq consult-root-function 'projectile-project-root)
  (setq consult-line-start-from-top nil))

(use-package consult-projectile
  :bind (("C-x p" . consult-projectile)))

(use-package consult-flycheck)

(setq consult-narrow-key "<")

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq which-key-use-C-h-commands nil)
  (setq embark-action-indicator
        (lambda (map _target)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(use-package embark-consult)

(use-package consult-eglot)
(use-package consult-eglot-embark
  :after consult-eglot embark
  :init (consult-eglot-embark-mode))

(define-key embark-symbol-map (kbd "h") 'helpful-symbol)

(define-key embark-general-map [?\r] 'projectile-find-file)

;(add-to-list 'marginalia-prompt-categories '("Find file" . file))


(setq completion-category-overrides nil)
(setq completion-category-defaults nil)

(setq completion-ignore-case t)

(defun johmue/consult-find-file-backward-kill (arg)
    (interactive "p")
  (if (and minibuffer-completing-file-name
	   (file-directory-p (minibuffer-contents)))
      (if (string-match-p "/." (minibuffer-contents))
	  (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (delete-backward-char arg)))

(defvar johmue/line-search-command 'consult-line)

(use-package ctrlf
  :bind (:map ctrlf-minibuffer-mode-map
              ("<down>" . ctrlf-next-match)
              ("<up>" . ctrlf-previous-match)
              ("C-<down>" . ctrlf-last-match)
              ("C-<up>" . ctrlf-first-match)
              ("M-<down>" . ctrlf-forward-alternate)
              ("M-<up>" . ctrlf-backward-alternate)
              ("C-S-s" . 'johmue/quit-search-and-consult-line))
  :init (ctrlf-mode 1))

(use-package visual-regexp
  :bind
  ("C-M-%" . vr/query-replace)
  ("C-%" . vr/replace))

(defun johmue/quit-search-and-consult-line ()
  (interactive)
  (put 'quit 'error-message "")
  (run-at-time nil nil
               (lambda ()
                 (put 'quit 'error-message "Quit")
                 (consult-line ctrlf--last-input)))
  (abort-recursive-edit))
