

(use-package tex
  :ensure auctex)

(use-package reftex)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list #'cape-tex
                              (cape-capf-super
                               #'capf-wordfreq-completion-at-point-function
                               #'cape-dabbrev
                               )))))

; latex-config.el ends here
