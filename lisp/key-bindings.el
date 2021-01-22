
(global-set-key (kbd "<S-backspace>") 'johmue/delete-whitespace-backward)
(global-set-key (kbd "<S-delete>") 'johmue/delete-whitespace-forward)
(global-set-key (kbd "<S-left>") 'johmue/jump-whitespace-backward)
(global-set-key (kbd "<S-right>") 'johmue/jump-whitespace-forward)

(global-set-key [(shift up)] 'beginning-of-defun)
(global-set-key [(shift down)] 'end-of-defun)

(global-set-key (kbd "<s-SPC>") 'johmue/mark-current-line)

(global-set-key (kbd "C-c C-l") 'johmue/eval-this-line)
(global-set-key (kbd "C-c C-d") 'eval-defun)

(global-set-key (kbd "C-c C-k") 'comment-region)
(global-set-key (kbd "C-c k") 'uncomment-region)

(global-set-key (kbd "C-c C-;") 'johmue/comment-current-line)
(global-set-key (kbd "C-c ;") 'johmue/uncomment-current-line)

(global-set-key (kbd "C-)") 'sp-wrap-round)
(global-set-key (kbd "C-]") 'sp-wrap-square)
(global-set-key (kbd "C-}") 'sp-wrap-curly)
(global-set-key (kbd "<C-S-delete>") 'sp-unwrap-sexp)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(define-key global-map [(meta up)] 'johmue/scroll-other-window-one-up)
(define-key global-map [(meta down)] 'johmue/scroll-other-window-one-down)

(define-key global-map [f9]          'copy-for-paste)
(define-key global-map [f10]         'paste-copied)

(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)

(global-set-key (kbd "C-x C-2") 'delete-other-windows-vertically)

(global-set-key (kbd "C-S-M-s") 'johmue/ripgrep-thing-at-point)

(global-set-key (kbd "M-q") 'johmue/fill-paragraph-79)

;(define-key global-map [f11]         'ispell-buffer)
;(define-key global-map [(meta f11)]  'flyspell-auto-correct-word)

;(define-key global-map '(meta control kp-right) 'bigframe)
;(define-key global-map '(meta control kp-6) 'bigframe)
;(define-key global-map '(meta control kp-left) 'normalframe)
;(define-key global-map '(meta control kp-4) 'normalframe)

(define-key global-map (kbd "C-c c") 'python-pytest-dispatch)
;(define-key python-mode-map [S-f12] 'python-pytest-repeat)

(define-key global-map [(meta g)] 'magit-status)

(define-key projectile-mode-map [f12] 'projectile-compile-project)
(define-key projectile-mode-map [S-f12] 'projectile-test-project)
(define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)


;;; keys.el ends here
