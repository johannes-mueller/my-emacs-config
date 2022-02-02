(use-package company
  :diminish
  :init (global-company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-complete)
	      ("<return>" . nil)
	      ("RET" . nil)
	      ("C-<return>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.2)
  (company-require-match nil)
  :config
  (setq company-ispell-dictionary (file-truename "~/.emacs.d/dicts/en.txt"))
  )

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1)
  (setq company-prescient-sort-length-enable t))

(use-package company-fuzzy
  :after company
  :config
  (setq company-fuzzy-prefix-on-top t)
  ;(setq company-fuzzy-sorting-function 'johmue/company-fuzzy-no-dabbrev)
)
(straight-use-package
 '(company-wordfreq :type git :host github :repo "johannes-mueller/company-wordfreq.el"))
