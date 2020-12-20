
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

(defun johmue/jump-to-project ()
  (projectile-dired)
  (projectile-vc)
  (other-window 1)
  (setq projectile-mode-line-function '(lambda () (format " <%s>" (projectile-project-name))))
)

(defun johmue/auto-activate-virtualenv ()
  (interactive)
  (let
      ((possible-env-dir
	(concat
	 (file-name-as-directory
	  (or (projectile-project-p) default-directory)) ".venv")))
    (message "Switching to %s" possible-env-dir)
    (if (file-directory-p possible-env-dir)
	(pyvenv-activate possible-env-dir)
      (pyvenv-deactivate))))


(defun johmue/update-elpa-keyring ()
  (interactive)
  (setq package-check-signature nil)
  (gnu-elpa-keyring-update)
  (setq package-check-signature 'allow-unsigned)
)



(provide 'johmue-defuns)

;;; johmue-defuns.el ends here
