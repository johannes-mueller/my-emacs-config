
(use-package vertico
  :ensure t
  :init (vertico-mode)
  :bind (:map minibuffer-local-map
              ("<backspace>" . johmue/consult-find-file-backward-kill)
	      ("S-<down>" . forward-paragraph)
	      ("S-<up>" . backward-paragraph)))

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
  :bind (("C-S-s" . consult-line)
	 ("C-S-r" . consult-ripgrep)
	 ("C-S-M-s" . johmue/isearch-line-symbol-at-point)
	 ("C-S-M-r" . johmue/ripgrep-symbol-at-point)
	 ("C-s-s" . consult-isearch-history)
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

(setq consult-narrow-key "<")

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

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

(define-key embark-symbol-map (kbd "h") 'helpful-symbol)

(embark-define-keymap embark-project-file-map
  "For projectile find file"
  :parent embark-file-map
  ("RET" projectile-find-file))

;(add-to-list 'marginalia-prompt-categories '("Find file" . file))


(use-package orderless
   :ensure t
   :custom
   (completion-styles '(orderless basic))
   (orderless-matching-styles '(orderless-prefixes)))


(setq completion-category-overrides nil)
(setq completion-category-defaults nil)

(setq completion-ignore-case t)

(use-package consult-lsp
  :after (lsp))

(defun johmue/consult-find-file-backward-kill (arg)
    (interactive "p")
  (if (and minibuffer-completing-file-name
	   (file-directory-p (minibuffer-contents)))
      (if (string-match-p "/." (minibuffer-contents))
	  (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (delete-backward-char arg)))

(defvar johmue/line-search-command 'consult-line)
