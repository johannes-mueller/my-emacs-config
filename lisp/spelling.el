;; -*- lexical-binding: t; -*-
;;; spelling.el --- Settings for hunspell

;;; Commentary:

;; Nothing special

;;; Code:

(setq-default ispell-program-name "hunspell")

;(eval-after-load "ispell"
;	'(progn (defun ispell-get-coding-system () 'utf-8)))

(use-package jinx
  ;:hook (emacs-startup . global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct))

(use-package auto-dictionary)
(add-hook 'jinx-mode-hook (lambda () (auto-dictionary-mode 1)))

(defun johmue/adjust-jinx-languages ()
  (setq-local jinx-languages
              (or (car (rassoc ispell-local-dictionary adict-dictionary-list))
                  ispell-local-dictionary))
  (jinx-mode -1)
  (jinx-mode 1))

(add-hook 'adict-change-dictionary-hook #'johmue/adjust-jinx-languages)

(setq adict-dictionary-list
      '(("en" . "english")
        ("de" . "deutsch")
        ("fr" . "francais")
        ("sv" . "svenska")
        ("eo" . "eo")
        ("sk" . "slovak")
        ("sl" . nil)))

;;; spelling.el ends here
