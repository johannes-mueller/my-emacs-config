
(use-package vertico
  :ensure t
  :init (vertico-mode)
  :bind (:map minibuffer-local-map
         ("<backspace>" . johmue/consult-find-file-backward-kill)))

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
  :bind (("C-S-s" . johmue/isearch-line-symbol-at-point)
	 ("C-S-r" . johmue/ripgrep-symbol-at-point)
	 ("s-s" . consult-line)
	 ("s-r" . consult-ripgrep)
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

(define-key embark-symbol-map (kbd "h") 'helpful-symbol)


(use-package orderless
   :ensure t
   :custom (completion-styles '(orderless)))

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
