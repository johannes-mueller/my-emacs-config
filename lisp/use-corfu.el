(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle nil)                ;; Disable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-at-boundary nil)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 1)
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; You may want to enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since dabbrev can be used globally (M-/).
  :init
  (global-corfu-mode)
  (corfu-prescient-mode -1)
  :hook ((multiple-cursors-mode . (lambda () (corfu-mode -1)))
         (corfu-mode . corfu-popupinfo-mode))
  :bind (:map corfu-map
              ("<prior>" . corfu-popupinfo-scroll-down)
              ("<next>" . corfu-popupinfo-scroll-up)
              ("<left>" . corfu-quit)
              ("<right>" . corfu-quit)
              ("<up>" . johmue/corfu-previous)
              ("C-SPC" . corfu-quit)
              ("RET" . corfu-complete)
              ("<end>" . corfu-complete)
              ("<tab>" . corfu-expand)
              ("s-<tab>" . corfu-complete)))
(add-hook 'multiple-cursors-mode-disabled-hook #'corfu-mode)

(straight-use-package
 '(capf-wordfreq :type git :host github :repo "johannes-mueller/capf-wordfreq.el"))

(setq capf-wordfreq-minimal-candidate-length 8)

(add-to-list 'completion-styles-alist
             '(tab completion-basic-try-completion ignore
               "Completion style which provides TAB completion only."))
(setq completion-styles '(tab prescient basic))


(use-package cape
  ;; Bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
)

;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

(setq completion-cycle-threshold 3)
(setq tab-always-indent 'complete)

(defun johmue/corfu-previous (&optional n)
  "Go backward N corfu candidates or corfu-quit when we are at the first."
  (interactive "p")
  (if (eq corfu--index 0)
      (corfu-quit)
    (corfu-next (- (or n 1)))))

(add-to-list 'corfu-continue-commands 'johmue/corfu-previous)

;; quit corfu on the following charsd
(dolist (c (list "." "," ":" ";" "(" ")" "{" "}" "[" "]" "|"))
  (define-key corfu-map (kbd c)
              `(lambda () (interactive) (corfu-quit) (insert ,c) (sp-insert-pair))))


(setq corfu-separator 32)

;; quit corfu when space is hit twice
(define-key corfu-map (kbd "SPC")
  (lambda ()
    (interactive)
    (if (= (char-before) corfu-separator)
        (progn
          (corfu-quit))
      (corfu-insert-separator))))

(setq global-company-mode nil)
