(require 'project)

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))


(defun johmue/detect-ws-backward ()
  "Delete whitespace until the previous non whitespace."
  (skip-chars-backward " \t\r\n"))

(defun johmue/detect-ws-forward ()
  "Delete whitespace until the previous non whitespace."
  (skip-chars-forward " \t\r\n"))

(defun johmue/delete-whitespace-impl (fn)
  (let ((here (point)))
    (funcall fn)
    (if (/= (point) here)
	(delete-region (point) here))))

(defun johmue/delete-whitespace-backward ()
  (interactive)
  (johmue/delete-whitespace-impl 'johmue/detect-ws-backward))

(defun johmue/delete-whitespace-forward ()
  (interactive)
  (johmue/delete-whitespace-impl 'johmue/detect-ws-forward))

(defun johmue/jump-whitespace-backward ()
  (interactive)
  (johmue/detect-ws-backward))

(defun johmue/jump-whitespace-forward ()
  (interactive)
  (johmue/detect-ws-forward))

(defun johmue/mc/insert-numbers-1 ()
    (interactive)
    (mc/insert-numbers 1))

(defun johmue/mc/insert-numbers-prompt (start)
    (interactive "nStart number: ")
    (mc/insert-numbers start))

(defun johmue/wrap-sexp-with (char)
  (let ((start-point (point)))
    (sp-wrap-with-pair char)
    (when (> start-point (point))
      (goto-char start-point)
      (right-char))))

(defun johmue/wrap-round ()
  (interactive)
  (johmue/wrap-sexp-with "("))

(defun johmue/wrap-square ()
  (interactive)
  (johmue/wrap-sexp-with "["))

(defun johmue/wrap-curly ()
  (interactive)
  (johmue/wrap-sexp-with "{"))

(defun johmue/wrap-single-quote ()
  (interactive)
  (johmue/wrap-sexp-with "'"))

(defun johmue/wrap-double-quote ()
  (interactive)
  (johmue/wrap-sexp-with "\""))

(defun johmue/attr-to-dict ()
  (sp-wrap-square)
  (sp-wrap-with-pair "\"")
  (left-char 2)
  (delete-char -1))

(defun johmue/dict-to-attr ()
  (sp-unwrap-sexp)
  (sp-unwrap-sexp)
  (insert "."))

(defun johmue/python-toggle-dict-attr ()
  (interactive)
  (save-excursion
    (forward-char)
    (sp-backward-sexp)
    (let ((c (char-before)))
      (cond ((eq c ?.) (johmue/attr-to-dict))
            ((or (eq c ?') (eq c ?\")) (johmue/dict-to-attr))))))

(defun johmue/downcase-char (arg)
  "Uppercasify ARG chars starting from point.  Point doesn't move."
  (interactive "p")
  (downcase-region (point) (save-excursion (forward-char arg) (point))))

(defun johmue/toggle-case ()
  (interactive)
  (let ((current-char (char-after)))
     (if (eq (upcase current-char) current-char)
      (johmue/downcase-char 1)
    (upcase-char 1))))

(defun johmue/eval-this-line ()
  (interactive)
  (save-excursion
    (eval-region (line-beginning-position) (line-end-position))))

(defun johmue/scroll-other-window-one-down ()
  (interactive)
  (scroll-other-window 1))

(defun johmue/scroll-other-window-one-up ()
  (interactive)
  (scroll-other-window-down 1))

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

(defun johmue/balance-windows-popper ()
  (interactive)
  (let ((open-popups (not (eq popper-open-popup-alist nil))))
    (when open-popups (popper-toggle))
    (balance-windows)
    (when open-popups (popper-toggle))))

(defun johmue/split-window-right ()
  (interactive)
  (split-window-right)
  (johmue/balance-windows-popper)
  (other-window 1))

(defun johmue/split-window-below ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun johmue/ripgrep-thing-at-point ()
  (interactive)
  (consult-ripgrep (projectile-project-root) (symbol-at-point)))

(defun johmue/adjust-python-shell-interpreter ()
  (let ((invoke-python (if (executable-find "ipython")
                           '("ipython" "--simple-prompt" "-i")
                         '("python" "-i"))))
    (setq python-shell-interpreter (pop invoke-python)
          python-shell-interpreter-args (string-join invoke-python " "))))

(defun johmue/activate-python-venv (env-dir)
  (pyvenv-activate env-dir)
  (johmue/adjust-python-shell-interpreter)
  (message "Switched to %s." env-dir))

(defun johmue/deactivate-python-venv ()
  (pyvenv-deactivate)
  (johmue/adjust-python-shell-interpreter)
  (message "Deactivated python environment."))

(defvar johmue/last-projectile-project-root nil)

(defun johmue/check-for-poetry-env ()
  (let ((venv (shell-command-to-string "poetry env info -p 2>/dev/null")))
    (when (length> venv 1)
      venv)))

(defun johmue/find-worktree-source ()
  (let ((dot-git-path (concat (projectile-project-root) ".git")))
    (when (file-regular-p dot-git-path)
      (with-temp-buffer
        (insert-file-contents dot-git-path)
        (when-let (endpoint (and (string-prefix-p "gitdir: " (buffer-string))
                                 (search-forward ".git" nil t)
                                 (match-beginning 0)))
          (buffer-substring 9 endpoint))))))

(defun johmue/check-for-venv (&optional project-root-dir)
  (when-let* ((project-root (or project-root-dir
                                (projectile-project-root)))
              (cand (concat project-root ".venv")))
    (when (file-directory-p cand) cand)))

(defun johmue/activate-or-deactivate-venv ()
  (pyvenv-deactivate)
  (if-let ((possible-env-dir (or (johmue/check-for-venv)
                                 (johmue/check-for-poetry-env)
                                 (johmue/check-for-venv (johmue/find-worktree-source)))))
      (johmue/activate-python-venv possible-env-dir)
    (johmue/adjust-python-shell-interpreter)
    (message "Deactivated python environment")))

(defun johmue/auto-activate-virtualenv (_buffer)
  (interactive)
  (let ((current-project-root (project-root (project-current))))
    (unless (equal current-project-root johmue/last-projectile-project-root)
      (johmue/activate-or-deactivate-venv)
      (setq johmue/last-projectile-project-root current-project-root))))

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
    (projectile-with-default-dir (projectile-acquire-root)
      (compile command))))

(defun johmue/sphinx-build-html-this-file ()
  (interactive)
  (let* ((docs-dir (concat (projectile-project-root) "docs"))
         (target-dir (concat (projectile-project-root) "_build/html"))
         (this-file (buffer-file-name))
         (command (concat "sphinx-build -b html -a " this-file " " docs-dir " " target-dir)))
    (projectile-with-default-dir (projectile-acquire-root)
      (compile command))))

(defun johmue/eldoc ()
  (interactive)
  (eldoc)
  (switch-to-buffer eldoc--doc-buffer))

(provide 'johmue-defuns)

;;; johmue-defuns.el ends here
