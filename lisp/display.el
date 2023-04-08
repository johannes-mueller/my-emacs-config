


(setq switch-to-buffer-in-dedicated-window 'pop)

(add-hook 'window-configuration-change-hook #'johmue/setup-display-buffer)

(defun johmue/setup-display-buffer ()
  (if (> (display-pixel-width) 2000)
      (johmue/big-screen-display)
    (johmue/small-screen-display)))

(defconst winpos/vterm-at-bottom
  '("\\*vterm.*\\*"
    (display-buffer-reuse-mode-window display-buffer-in-side-window)
    (inhibit-same-window . nil)
    (window-height . 0.3)
    (side . bottom)
    (mode vterm-mode vterm-copy-mode)))

(defun johmue/big-screen-display ()
  (setq display-buffer-alist
        `(("\\*compilation\\*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . right)
           (window-width . 0.33))
          ,winpos/vterm-at-bottom)))

(defun johmue/small-screen-display ()
  (setq display-buffer-alist
        `(("\\*compilation\\*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom)
           (window-height . 0.33))
          ("test_.*\\.py"
           display-buffer-reuse-window
           (direction . leftmost)
           (window-width . 0.5)
           (reusable-frames . t))
          ,winpos/vterm-at-bottom)))

(defun johmue/project-vterm ()
  (interactive)
  (if-let ((buf (get-buffer
                 (projectile-generate-process-name "vterm" nil
                                                   (projectile-acquire-root)))))
      (let ((window (or (get-buffer-window buf)
                        (display-buffer buf))))
        (select-window window))
    (projectile-run-vterm)))

(defconst johmue/regex-popup-window
  (rx (| "*xref*"
         "*grep*"
         "*Occur*"
         (seq "*vterm" (* any) "*")
         "*compilation*"
         "*Backtrace*")))

(defun johmue/close-popup-windows ()
  (mapcar (lambda (buf) (when buf (delete-window buf)))
          (mapcar #'get-buffer-window
                  (seq-filter (lambda (buf)
                                (string-match-p johmue/regex-popup-window
                                                (buffer-name buf)))
                              (buffer-list)))))

(advice-add 'keyboard-quit :before #'johmue/close-popup-windows)
