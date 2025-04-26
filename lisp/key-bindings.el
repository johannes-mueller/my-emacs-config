
(global-unset-key (kbd "C-\\"))

(define-key prog-mode-map (kbd "S-SPC") 'johmue/space-after-sexp)
(define-key prog-mode-map (kbd "S-<return>") 'johmue/return-after-sexp)

(global-set-key (kbd "<S-backspace>") 'johmue/delete-whitespace-backward)
(global-set-key (kbd "<S-delete>") 'johmue/delete-whitespace-forward)
(global-set-key (kbd "<S-left>") 'johmue/jump-whitespace-backward)
(global-set-key (kbd "<S-right>") 'johmue/jump-whitespace-forward)

(global-set-key (kbd "S-<home>") 'sp-backward-up-sexp)
(global-set-key (kbd "S-<end>") 'sp-up-sexp)

(global-set-key [(shift up)] 'beginning-of-defun)
(global-set-key [(shift down)] 'end-of-defun)

(global-set-key (kbd "C-c C-l") 'johmue/eval-this-line)
(global-set-key (kbd "C-c C-d") 'eval-defun)

(global-set-key (kbd "C-c d") 'johmue/eldoc)

(global-set-key (kbd "C-M-(") 'johmue/wrap-round)
(global-set-key (kbd "C-M-[") 'johmue/wrap-square)
(global-set-key (kbd "C-M-{") 'johmue/wrap-curly)
(global-set-key (kbd "C-M-\"") 'johmue/wrap-double-quote)
(global-set-key (kbd "C-M-'") 'johmue/wrap-single-quote)
(global-set-key (kbd "<C-S-delete>") 'sp-unwrap-sexp)

(global-set-key (kbd "C-s-[") 'johmue/python-toggle-dict-attr)

(global-set-key (kbd "C-M-c") 'johmue/toggle-case)
(define-key minibuffer-mode-map (kbd "C-M-c")'exit-recursive-edit)

(global-set-key (kbd "M-d") 'duplicate-dwim)

(define-key global-map (kbd "M-S-<up>") 'johmue/scroll-other-window-one-up)
(define-key global-map (kbd "M-S-<down>") 'johmue/scroll-other-window-one-down)

(define-key global-map (kbd "M-<up>") (lambda () (interactive) (save-excursion (scroll-up-line 1))))
(define-key global-map (kbd "M-<down>") (lambda () (interactive) (save-excursion (scroll-down-line 1))))

(define-key global-map [f9]          'copy-for-paste)
(define-key global-map [f10]         'paste-copied)

(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)

(global-set-key (kbd "C-x C-2") 'delete-other-windows-vertically)
(global-set-key (kbd "C-x 3") 'johmue/split-window-right)
(global-set-key (kbd "C-x 2") 'johmue/split-window-below)

(global-set-key (kbd "C-x C-r") 'consult-recent-file)
(global-set-key (kbd "C-l") 'consult-goto-line)

(define-key isearch-mode-map (kbd "<up>") 'isearch-repeat-backward)
(define-key isearch-mode-map (kbd "<down>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-s") 'johmue/change-isearch-to-line-search)

(global-set-key (kbd "s-/") 'completion-at-point)
(global-set-key (kbd "s-<tab>") 'completion-at-point)

(define-key text-mode-map (kbd "M-q") 'johmue/fill-paragraph-79)
(define-key python-ts-mode-map (kbd "M-q") (lambda () (interactive) (python-black-partial-dwim nil)))
(define-key python-ts-mode-map (kbd "<f5>") 'johmue/python-black-format-defun)
(define-key python-ts-mode-map (kbd "S-<end>") 'sp-up-sexp)
(define-key python-ts-mode-map (kbd "S-<home>") 'sp-backward-up-sexp)


(global-set-key [f6] 'johmue/toggle-soft-wrap)

(global-set-key (kbd "M-$") 'jinx-correct)
;(define-key global-map '(meta control kp-right) 'bigframe)
;(define-key global-map '(meta control kp-6) 'bigframe)
;(define-key global-map '(meta control kp-left) 'normalframe)
;(define-key global-map '(meta control kp-4) 'normalframe)

(define-key global-map (kbd "C-c c") 'python-pytest-dispatch)
;(define-key python-mode-map [S-f12] 'python-pytest-repeat)

(define-key global-map [(meta g)] 'magit-status)

(define-key projectile-mode-map [S-f12] 'test-cockpit-test-or-projectile-test)
(define-key projectile-mode-map [f12] 'test-cockpit-repeat-test-or-projectile-test)
(define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)

(define-key dired-mode-map (kbd "?") #'which-key-show-major-mode)
(define-key dired-mode-map (kbd "<left>") #'dired-up-directory)
(define-key dired-mode-map (kbd "<right>") #'dired-find-file)
(define-key dired-mode-map (kbd "h") #'dired-gitignore-global-mode)

(global-set-key (kbd "C-c C-s") #'yas-insert-snippet)
;;; keys.el ends here
