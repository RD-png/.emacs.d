;;; ui.el -*- lexical-binding: t; -*-

(setq confirm-nonexistent-file-or-buffer nil)
(setq uniquify-buffer-name-style 'forward
      ring-bell-function #'ignore
      visible-bell nil)

(setq mouse-yank-at-point t)

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;;; Cursor.
(blink-cursor-mode -1)
(setq blink-matching-paren nil)
(setq x-stretch-cursor nil)


;;; Fringes.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)


;;; Line Numbers.
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)
(dolist (mode '(prog-mode-hook conf-mode-hook))
  (add-hook mode #'display-line-numbers-mode))


;;; Windows.
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise nil)

(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      tooltip-mode nil)

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'after-init-hook #'window-divider-mode)

(setq use-dialog-box nil)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))


;;; Minibuffers.
(setq enable-recursive-minibuffers t)
(setq echo-keystrokes 0.02)
(advice-add #'yes-or-no-p :override #'y-or-n-p)


;;; Misc.
(setq inhibit-message nil)
(when (window-system) (setq confirm-kill-emacs 'yes-or-no-p))
(advice-add 'display-startup-echo-area-message :override #'ignore)


;;; Built-in.
(setq ansi-color-for-comint-mode t)

(use-package comint
  :straight (comint :type built-in)
  :config
  (setq comint-prompt-read-only t
        comint-buffer-maximum-size 2048))

(use-package compile
  :straight (compile :type built-in)
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error)
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer))

(use-package window
  :straight (window :type built-in)
  :config
  (setq display-buffer-alist
        `(("\\*\\(scheme\\|eshell\\|vterm\\)\\*"
           (display-buffer-in-side-window)
           (dedicated . t)
           (side . bottom)
           (slot . 0))
          ("\\*Embark Actions\\*"
           (display-buffer-reuse-mode-window display-buffer-at-bottom)
           (window-height . fit-window-to-buffer)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . none))))))
  (setq switch-to-buffer-in-dedicated-window 'pop)
  (setq window-sides-vertical nil)
  (setq window-combination-resize t))

(use-package ediff
  :straight (ediff :type built-in)
  :config
  (setq ediff-diff-options "-w"
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package paren
  :straight (paren :type built-in)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  :init
  (show-paren-mode 1))

(setq whitespace-line-column nil
      whitespace-style
      '(face indentation tabs tab-mark spaces space-mark newline newline-mark
        trailing lines-tail)
      whitespace-display-mappings
      '((tab-mark ?\t [?› ?\t])
        (newline-mark ?\n [?¬ ?\n])
        (space-mark ?\  [?·] [?.])))


;;; External.
(use-package highlight-numbers
  :straight t
  :hook ((prog-mode conf-mode) . highlight-numbers-mode)
  :config (setq highlight-numbers-generic-regexp "\\_<[[:digit:]]+\\(?:\\.[0-9]*\\)?\\_>"))

(setq image-animate-loop t)

(use-package dashboard
  :straight t
  :bind
  ("C-x c e" . dashboard-refresh-buffer)
  :config
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 5)))
  (setq dashboard-banner-logo-title "")
  (setq dashboard-set-file-icons t)
  :init
  (dashboard-setup-startup-hook))

(use-package popper
  :straight t
  :bind (("C-c C-." . popper-toggle-latest)
         ("C-c M-." . popper-kill-latest-popup)
         ("C-c C-/" . popper-cycle)
         ("C-c C-;" . popper-toggle-type))
  :init
  (setq popper-window-height 15)
  (setq even-window-sizes nil)
  (setq display-buffer-base-action
        '(display-buffer-reuse-mode-window
          display-buffer-reuse-window
          display-buffer-same-window))
  (setq popper-reference-buffers
        (append
         '("\\*Messages\\*"
           "\\*scheme\\*"
           "\\*erlang\\*"
           "\\*eshell\\*"
           "\\*vterm\\*"
           "^\\*Warnings\\*$"
           "Output\\*$"
           "^\\*Backtrace\\*"
           "\\*Async Shell Command\\*"
           "\\*Completions\\*"
           "\\*devdocs\\*"
           "[Oo]utput\\*"
           "*helpful command: *.*$"
           "*helpful function: *.*$"
           "*helpful variable: *.*$"
           help-mode
           compilation-mode)))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package all-the-icons
  :straight t
  :config
  (setq all-the-icons-scale-factor 1))

(use-package all-the-icons-dired
  :straight t
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil))

(use-package diredfl
  :disabled t
  :straight t
  :hook (dired-mode . diredfl-mode)
  :init
  (setq diredfl-ignore-compressed-flag nil)
  (diredfl-global-mode 1))

(use-package visual-fill-column
  :straight t
  :hook (Info-mode . visual-fill-column-mode)
  :init
  (setq visual-fill-column-center-text t
        visual-fill-column-width 75))

(provide 'ui)
