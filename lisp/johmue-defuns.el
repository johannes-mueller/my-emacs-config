
(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))


(defun johmue/detect-ws-backward ()
  "Delete whitespace until the previous non whitespace."
  (skip-chars-backward " \t\r\n")
  )

(defun johmue/detect-ws-forward ()
  "Delete whitespace until the previous non whitespace."
  (skip-chars-forward " \t\r\n")
  )

(defun johmue/delete-whitespace-impl (fn)
  (let ((here (point)))
    (funcall fn)
    (if (/= (point) here)
	(delete-region (point) here))
    ))

(defun johmue/delete-whitespace-backward ()
  (interactive)
  (johmue/delete-whitespace-impl 'johmue/detect-ws-backward)
)

(defun johmue/delete-whitespace-forward ()
  (interactive)
  (johmue/delete-whitespace-impl 'johmue/detect-ws-forward)
)

(defun johmue/jump-whitespace-backward ()
  (interactive)
  (johmue/detect-ws-backward)
)

(defun johmue/jump-whitespace-forward ()
  (interactive)
  (johmue/detect-ws-forward)
)

(defun johmue/mark-current-line ()
  (interactive)
  (beginning-of-line)
  (set-mark (line-end-position))
  )

(defun johmue/eval-this-line ()
  (interactive)
  (save-excursion
    (eval-region (line-beginning-position) (line-end-position))
    )
  )

(defun johmue/comment-current-line ()
  (interactive)
  (save-excursion
    (comment-region (line-beginning-position) (line-end-position))
    )
  )

(defun johmue/uncomment-current-line ()
  (interactive)
  (save-excursion
    (uncomment-region (line-beginning-position) (line-end-position))
    )
  )

(defun johmue/scroll-other-window-one-down ()
  (interactive)
  (scroll-other-window 1)
  )

(defun johmue/scroll-other-window-one-up ()
  (interactive)
  (scroll-other-window-down 1)
  )

(defun johmue/indent-and-maybe-yas-expand ()
  (interactive)
  (indent-for-tab-command)
  (if (not (looking-at "[[:alnum:]]"))
      (yas-expand)))

(defun johmue/jump-to-project ()
  (projectile-dired)
  (projectile-vc)
  (other-window 1)
  (setq projectile-mode-line-function '(lambda () (format " <%s>" (projectile-project-name))))
)

(defun johmue/ripgrep-thing-at-point ()
  (interactive)
  (counsel-rg (ivy-thing-at-point) (projectile-project-root)))

(defun johmue/adjust-python-shell-interpreter ()
  (let ((invoke-python (if (executable-find "ipython")
			   '("ipython" "--simple-prompt" "-i")
			 '("python" "-i"))))
  (setq python-shell-interpreter (pop invoke-python)
	python-shell-interpreter-args (pop invoke-python))))

(defun johmue/activate-python-venv (env-dir)
  (pyvenv-activate env-dir)
  (johmue/adjust-python-shell-interpreter)
  (message "Switched to %s." env-dir)
  )

(defun johmue/deactivate-python-venv ()
  (pyvenv-deactivate)
  (johmue/adjust-python-shell-interpreter)
  (message "Deactivated python environment."))

(defun johmue/auto-activate-virtualenv ()
  (interactive)
  (let
      ((possible-env-dir
	(concat
	 (file-name-as-directory
	  (or (projectile-project-p) default-directory)) ".venv")))
    (if (file-directory-p possible-env-dir)
	(johmue/activate-python-venv possible-env-dir)
      (johmue/deactivate-python-venv))))

(defvar johmue/company-fuzzy-allowed-backends '("capf"))

(defun johmue/company-fuzzy-no-dabbrev (candidates)
  (let ((preferred-candidates '()))
    (dolist (cand candidates)
     (let* ((backend (company-fuzzy--get-backend-by-candidate cand))
	    (prefix (company-fuzzy--backend-prefix-candidate cand 'match)))
       (when (or (company-fuzzy--string-prefix-p prefix cand)
		 (member backend johmue/company-fuzzy-allowed-backends))
	 (push cand preferred-candidates)
	 (setq candidates (remove cand candidates)))))
    (append preferred-candidates candidates)))

(defun johmue/unfill-paragraph (&optional region)
      (interactive)
      (let ((fill-column (point-max)))
        (fill-paragraph nil region)))

(defun johmue/fill-paragraph-79 ()
  "Formats the paragraph to 79 characters independently of the fill-column setting."
  (interactive)
  (let ((fill-column 79))
    (fill-paragraph)))

(defun johmue/toggle-soft-wrap ()
  (interactive)
  (auto-fill-mode 'toggle)
  (visual-line-mode 'toggle)
  (visual-fill-column-mode 'toggle))

(provide 'johmue-defuns)

;;; johmue-defuns.el ends here
