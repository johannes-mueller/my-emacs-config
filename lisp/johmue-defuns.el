
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

(defun johmue/yas-expand ()
  (interactive)
  (corfu-quit)
  (yas-expand))

(defun johmue/indent-and-maybe-yas-expand ()
  (interactive)
  (indent-for-tab-command)
  (let* ((keys (recent-keys))
         (last-key (aref keys (- (length keys) 3))))
    (if (integerp last-key)
        (yas-expand))))


(defun johmue/jump-to-project ()
  (projectile-dired)
  (projectile-vc)
  (other-window 1)
  (setq projectile-mode-line-function '(lambda () (format " <%s>" (projectile-project-name))))
)

(defun johmue/split-window-right ()
  (interactive)
  (split-window-right)
  (balance-windows))

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

(defvar johmue/last-projectile-project-root nil)

(defun johmue/auto-activate-virtualenv ()
  (interactive)
  (let ((project-root (projectile-project-root)))
    (unless (equal project-root johmue/last-projectile-project-root)
      (let ((possible-env-dir
             (concat
              (file-name-as-directory (or project-root default-directory))
              ".venv")))
        (if (file-directory-p possible-env-dir)
            (johmue/activate-python-venv possible-env-dir)
          (johmue/deactivate-python-venv))
        (setq johmue/last-projectile-project-root project-root)))))

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

(defun johmue/start-web-server ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (start-process-shell-command "Webserver" "*Webserver*" "python -m http.server 8080")))

(defun johmue/start-jupyter-server ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (start-process-shell-command "Jupyter" "*Jupyter*" "jupyter notebook")))

(defun johmue/search--search-initialize (&optional backward)
  (phi-search--initialize phi-search-mode-line-format
                          (if backward
                              phi-search-additional-keybinds
                            phi-search-additional-keybinds)
                          nil
                          (when backward 'phi-search--backward-after-update-function)
                          'phi-search--complete-function
                          nil
                          (lambda ()
                            (run-hooks 'phi-search-init-hook))))


(defun johmue/isearch ()
  (interactive)
  (johmue/search--search-initialize nil))

(defun johmue/isearch-backward ()
  (interactive)
  (johmue/search--search-initialize t))

(defun johmue/change-isearch-to-line-search ()
  (interactive)
  (put 'quit 'error-message "")
  (run-at-time nil nil
               (lambda ()
                 (put 'quit 'error-message "Quit")
                 (funcall johmue/line-search-command isearch-string)))
  (isearch-abort))

(defun johmue/isearch-line-symbol-at-point ()
  (interactive)
  (funcall johmue/line-search-command (thing-at-point 'symbol)))

(defun johmue/ripgrep-symbol-at-point()
  (interactive)
  (consult-ripgrep (projectile-project-root) (thing-at-point 'symbol)))


(defun johmue/sphinx-build-html ()
  (interactive)
  (let* ((docs-dir (concat (projectile-project-root) "docs"))
         (target-dir (concat (projectile-project-root) "_build/html"))
         (command (concat "sphinx-build -b html " docs-dir " " target-dir)))
    (compile command)))


(defun johmue/sphinx-build-html-this-file ()
  (interactive)
  (let* ((docs-dir (concat (projectile-project-root) "docs"))
         (target-dir (concat (projectile-project-root) "_build/html"))
         (this-file (buffer-file-name))
         (command (concat "sphinx-build -b html -a " this-file " " docs-dir " " target-dir)))
    (compile command)))


(provide 'johmue-defuns)

;;; johmue-defuns.el ends here
