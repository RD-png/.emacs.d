;;; my-theme.el -*- lexical-binding: t; -*-

;;; Font.
(defvar default-font-size 130)
(defvar default-variable-font-size 130)
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
  :config
  ;; Override default face
  (custom-theme-set-faces
   'modus-operandi
   '(vertico-current ((t (:background "light blue")))))
  :init
  (setq  modus-themes-intense-hl-line t
         modus-themes-org-blocks 'grayscale
         modus-themes-scale-headings t
         modus-themes-section-headings nil
         modus-themes-variable-pitch-headings nil
         modus-themes-intense-paren-match t
         modus-themes-diffs 'desaturated
         modus-themes-syntax '(green-strings yellow-comments)
         modus-themes-links '(faint neutral-underline)
         modus-themes-hl-line '(intense)
         modus-themes-prompts '(bold background)
         modus-themes-mode-line '(accented borderless)
         modus-themes-subtle-line-numbers t
         modus-themes-tabs-accented t
         modus-themes-inhibit-reload t
         modus-themes-paren-match '(underline)
         modus-themes-region '(no-extend accented bg-only)
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
        '((bg-main . "#FFFFE8")
          (fg-comment-yellow . "Firebrick")
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
  :custom
  (doom-modeline-height 10)
  (doom-modeline-bar-width 1)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-github-timer nil)
  (doom-modeline-gnus-timer nil)
  :config
  (setq doom-modeline-buffer-modification-icon nil)
  (setq doom-modeline-hud t)
  (setq doom-modeline-lsp nil)
  :init
  (doom-modeline-mode +1))

(provide 'my-theme)
