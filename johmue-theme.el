
(require 'autothemer)
(require 'rainbow-delimiters)

(autothemer-deftheme johmue "Johannes's theme"
 ((((class color) (min-colors #xFFFFFF)))
  (johmue-black "black")
  (johmue-white "LightYellow")
  (johmue-red "red3")
  (johmue-dark-red "red4")
  (johmue-magenta "magenta4")
  (johmue-green "green4")
  (johmue-cyan "cyan4")
  (johmue-blue "blue3")
  (johmue-dark-blue "blue4")
  (johmue-yellow "yellow4")
  (johmue-orange "orange4")
  (johmue-alert "red")
  (johmue-comment "rosybrown")
  (johmue-region "DeepSkyBlue")
  (johmue-hl-line "WhiteSmoke")
  )

 ((default (:foreground johmue-black :background johmue-white))
  (cursor (:background johmue-black))
  (region (:background johmue-region))
  (hl-line (:background johmue-hl-line))

  (font-lock-keyword-face (:foreground johmue-red))
  (font-lock-doc-string-face (:foreground johmue-magenta))
  (font-lock-reference-face (:foreground johmue-dark-red))
  (font-lock-string-face (:foreground johmue-green))
  (font-lock-function-name-face (:foreground johmue-dark-blue :bold t))
  (font-lock-type-face (:foreground johmue-magenta :bold t))
  (font-lock-comment-face (:foreground johmue-comment))
  (font-lock-constant-face (:foreground johmue-magenta))
  (font-lock-variable-name-face (:foreground johmue-dark-blue))

  (rainbow-delimiters-depth-1-face (:foreground "black"))
  (rainbow-delimiters-depth-2-face (:foreground "dark red"))
  (rainbow-delimiters-depth-3-face (:foreground "dark olive green"))
  (rainbow-delimiters-depth-4-face (:foreground "dark blue"))
  (rainbow-delimiters-depth-5-face (:foreground "dark orchid"))
  (rainbow-delimiters-depth-6-face (:foreground "deep sky blue"))
  (rainbow-delimiters-depth-7-face (:foreground "magenta"))
  (rainbow-delimiters-depth-8-face (:foreground "lime green"))
  (rainbow-delimiters-depth-9-face (:foreground "deep pink"))
  (rainbow-delimiters-mismatched-face (:forground johmue-alert :bold t))
  (rainbow-delimiters-unmatched-face (:foreground "red"))

  (vterm-color-black (:foreground "black"))
  (vterm-color-blue (:foreground johmue-blue))
  (vterm-color-cyan (:foreground johmue-cyan))
  (vterm-color-green (:foreground johmue-green))
  (vterm-color-magenta (:foreground johmue-magenta))
  (vterm-color-red (:foreground johmue-red))
  (vterm-color-yellow (:foreground johmue-yellow))
  (vterm-color-white (:foreground johmue-white))

  (highlight-indent-guides-character-face (:foreground "darkgray"))

  (line-number              (:foreground "dim gray" :background "white smoke"))
  (line-number-current-line (:foreground "dim gray" :background "white"))
  (auto-dim-other-buffers-face (:background "white smoke"))

  (web-mode-html-tag-face (:foreground johmue-dark-blue :bold t))
  (web-mode-html-attr-name-face (:foreground johmue-magenta))
  )

 (custom-theme-set-variables 'johmue
			     `(ansi-color-names-vector ["black"
							,johmue-red
							,johmue-green
							,johmue-yellow
							,johmue-blue
							,johmue-magenta
							,johmue-orange
							,johmue-cyan])))
(provide-theme 'johmue)
;;; johmue-theme.el ends here
