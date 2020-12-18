;;; spelling.el --- Settings for hunspell

;;; Commentary:

;; Nothing special

;;; Code:

(setq-default ispell-program-name "hunspell")

;(eval-after-load "ispell"
;        '(progn (defun ispell-get-coding-system () 'utf-8)))

(use-package auto-dictionary)
(add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1)))


;;; spelling.el ends here
