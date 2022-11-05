;;; my-theme.el -*- lexical-binding: t; -*-

;;; Font.
(defvar default-font-size 120)
(defvar default-variable-font-size 120)
(set-face-attribute 'default nil
                    :font "Fantasque Sans Mono"
                    :foundry "PfEd"
                    :slant 'normal
                    :weight 'normal
                    :width 'normal
                    :height default-font-size)

(set-face-attribute 'fixed-pitch nil
                    :font "Fantasque Sans Mono"
                    :height default-font-size)

(set-face-attribute 'variable-pitch nil
                    :font "Fantasque Sans Mono"
                    :height default-variable-font-size
                    :weight 'regular)


;;; Theme.
(setq custom-safe-themes t)
(setq custom--inhibit-theme-enable nil)
(setq x-underline-at-descent-line t)
(set-face-attribute 'cursor nil :background "IndianRed3")

(use-package modus-themes
  :straight t
  :config
  (custom-theme-set-faces
   'modus-operandi
   '(vertico-current ((t (:background "light blue"))))
   '(fill-column-indicator ((t (:foreground "light blue")))))
  :init
  (setq  modus-themes-intense-hl-line t
         modus-themes-org-blocks 'grayscale
         modus-themes-scale-headings t
         modus-themes-section-headings nil
         modus-themes-variable-pitch-headings nil
         modus-themes-intense-paren-match t
         modus-themes-diffs 'desaturated
         modus-themes-lang-checkers '(straight-underline)
         modus-themes-syntax '(green-strings yellow-comments)
         modus-themes-links '(faint neutral-underline)
         modus-themes-hl-line '(intense)
         modus-themes-prompts '(bold background)
         modus-themes-mode-line '(3d accented borderless)
         modus-themes-subtle-line-numbers t
         modus-themes-tabs-accented t
         modus-themes-inhibit-reload t
         modus-themes-paren-match '(underline intense)
         modus-themes-region '(no-extend accented bg-only)
         modus-themes-completions '((matches . (extrabold))
                                    (selection . (semibold accented))
                                    (popup . (accented faint)))
         modus-themes-org-agenda
         '((header-block . (variable-pitch scale-title))
           (header-date . (bold-today grayscale scale))
           (scheduled . rainbow)
           (habit . traffic-light-deuteranopia))
         modus-themes-headings  '((t . (background overline rainbow)))
         modus-themes-variable-pitch-ui nil
         modus-themes-scale-vheadings t
         modus-themes-scale-1 1.1
         modus-themes-scale-2 1.15
         modus-themes-scale-3 1.20
         modus-themes-scale-4 1.25
         modus-themes-scale-title 1.30)
  (setq modus-themes-operandi-color-overrides
        '((fg-comment-yellow . "Firebrick")
          ;; (bg-main . "#FFFFE8")
          (bg-main . "#f4f0ec")
          (bg-dim . "#F8F8F8")
          (bg-alt . "#E8E8E8")))
  (load-theme 'modus-operandi t))

(use-package ligature
  :straight (ligature :host github :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  :config
  (global-ligature-mode +1))

(use-package doom-modeline
  :straight t
  ;; :disabled t
  :custom
  (doom-modeline-height 10)
  :config
  (setq doom-modeline-buffer-modification-icon nil)
  (setq doom-modeline-hud t)
  (setq doom-modeline-lsp nil)

  (defun doom-modeline-update-pdf-pages ()
    "Update PDF pages."
    (setq doom-modeline--pdf-pages
          (let ((current-page-str (number-to-string (eval `(pdf-view-current-page))))
                (total-page-str (number-to-string (pdf-cache-number-of-pages))))
            (concat
             (propertize
              (concat (make-string (- (length total-page-str) (length current-page-str)) ? )
                      " P" current-page-str)
              'face 'mode-line)
             (propertize (concat "/" total-page-str) 'face 'doom-modeline-buffer-minor-mode)))))

  (doom-modeline-def-segment pdf-pages
    "Display PDF pages."
    (if (doom-modeline--active) doom-modeline--pdf-pages
      (propertize doom-modeline--pdf-pages 'face 'mode-line-inactive)))

  (doom-modeline-def-modeline 'pdf
    '(bar window-number pdf-pages buffer-info)
    '(misc-info matches major-mode process vcs))
  :init
  (doom-modeline-mode +1))

(use-package nano-modeline
  :straight t
  :disabled t
  :config
  (setq nano-modeline-prefix 'status)
  (setq nano-modeline-prefix-padding 1)

  (defun my/thin-modeline ()
  "Transform the modeline in a thin faded line"

  (nano-modeline-face-clear 'mode-line)
  (nano-modeline-face-clear 'mode-line-inactive)
  (setq mode-line-format (list ""))
  (setq-default mode-line-format (list ""))
  (set-face-attribute 'mode-line nil
                      :inherit 'nano-modeline-inactive
                      :height 0.1)
  (set-face-attribute 'mode-line-inactive nil
                      :inherit 'nano-modeline-inactive
                      :height 0.1))

  (add-hook 'nano-modeline-mode-hook #'my/thin-modeline)

  (set-face-attribute 'header-line nil)
  (set-face-attribute 'nano-modeline-active-name nil
                      :foreground "black"
                      :inherit '(nano-modeline-active nano-strong))
  (set-face-attribute 'nano-modeline-active-primary nil
                      :inherit '(nano-modeline-active))
  (set-face-attribute 'nano-modeline-active-secondary nil
                      :inherit '(nano-faded nano-modeline-active))

  (set-face-attribute 'nano-modeline-active-status-RW nil
                      :inherit '(nano-faded-i nano-strong nano-modeline-active))
  (set-face-attribute 'nano-modeline-active-status-** nil
                      :inherit '(nano-popout-i nano-strong nano-modeline-active))
  (set-face-attribute 'nano-modeline-active-status-RO nil
                      :inherit '(nano-default-i nano-strong nano-modeline-active))

  (set-face-attribute 'nano-modeline-inactive-name nil
                      :inherit '(nano-faded nano-strong
                                            nano-modeline-inactive))
  (set-face-attribute 'nano-modeline-inactive-primary nil
                      :inherit '(nano-faded nano-modeline-inactive))

  (set-face-attribute 'nano-modeline-inactive-secondary nil
                      :inherit '(nano-faded nano-modeline-inactive))
  (set-face-attribute 'nano-modeline-inactive-status-RW nil
                      :inherit '(nano-modeline-inactive-secondary))
  (set-face-attribute 'nano-modeline-inactive-status-** nil
                      :inherit '(nano-modeline-inactive-secondary))
  (set-face-attribute 'nano-modeline-inactive-status-RO nil
                      :inherit '(nano-modeline-inactive-secondary))

  ;; (set-face-attribute 'window-divider nil
  ;;                     :foreground (face-background 'default))

  ;; (set-face-attribute 'window-divider-first-pixel nil
  ;;                     :foreground (face-background 'default))

  ;; (set-face-attribute 'window-divider-last-pixel nil
  ;;                     :foreground (face-background 'default))
  :init
  (nano-modeline-mode 1))

(provide 'my-theme)
