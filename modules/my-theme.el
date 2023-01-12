;;; my-theme.el -*- lexical-binding: t; -*-

;;; Font.
(defvar default-font-size 120)
(defvar default-variable-font-size 110)
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
  :straight (modus-themes :host github :repo "protesilaos/modus-themes")
  :init
  (setq modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t
        modus-themes-org-blocks 'gray-background
        modus-themes-prompts '(bold background)
        modus-themes-headings '((1 . (variable-pitch 1.15))
                                (2 . (rainbow 1.1))
                                (t . (semibold)))
        modus-themes-region '(no-extend accented bg-only)
        modus-themes-common-palette-overrides '((fringe unspecified)
                                                (bg-line-number-active unspecified)
                                                (bg-line-number-inactive unspecified)
                                                (fg-line-number-active "#0000b0")

                                                (border-mode-line-active unspecified)
                                                (border-mode-line-inactive unspecified)

                                                (underline-paren-match fg-main)))
  (load-theme 'modus-operandi-tinted t))

(use-package standard-themes
  :straight (standard-themes :host github :repo "protesilaos/standard-themes")
  :disabled t
  :custom-face
  (default ((t (:background "#f4f0ec"))))
  (fringe ((t (:background "#f4f0ec"))))
  :init
  (load-theme 'standard-light t))

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
  :disabled t
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
  :init
  (nano-modeline-mode 1))

(use-package smart-mode-line
  :straight t
  :disabled t
  :commands sml/setup
  :init
  (setq sml/theme nil)
  (sml/setup)
  :config
  (setq sml/show-file-name nil)
  (setq mode-line-default-help-echo nil
        show-help-function nil)
  (add-to-list 'sml/replacer-regexp-list '("^~/[dD]ocuments/[rR]oam.*/" ":ROAM:")))

(use-package minions
    :custom
    (minions-prominent-modes '(flymake-mode))
    :init
    (minions-mode +1))

(provide 'my-theme)
