;; -*- lexical-binding: t; -*-

;; GC Config
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(setq read-process-output-max 1048576)

(custom-set-variables
 '(warning-suppress-log-types '((org-element-cache))))

(defun my/defer-garbage-collection ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my/restore-garbage-collection ()
  (run-at-time 1 nil (lambda () (setq gc-cons-threshold 16777216))))

(add-hook 'minibuffer-setup-hook 'my/defer-garbage-collection)
(add-hook 'minibuffer-exit-hook 'my/restore-garbage-collection)
(add-hook 'emacs-startup-hook #'emacs-init-time)

;; Native Comp
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq comp-deferred-compilation t)
    (setq warning-minimum-level :error)
    (setq package-native-compile t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))))
(setq load-prefer-newer t)

;; Paths
(push "node_modules/" completion-ignored-extensions)
(push "__pycache__/" completion-ignored-extensions)
(add-to-list 'exec-path "~/.npm/bin")

;; Defaults
(setq undo-limit 80000000
      delete-old-versions t
      delete-by-moving-to-trash t
      enable-recursive-minibuffers t
      scroll-conservatively 100
      scroll-preserve-screen-position t
      system-uses-terminfo nil
      kill-do-not-save-duplicates t
      sentence-end-double-spacev nil
      make-backup-files nil
      backup-inhibited t
      auto-save-default nil
      create-lockfiles nil
      initial-scratch-message ""
      uniquify-buffer-name-style 'forward)
(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

;; Interface
(global-font-lock-mode t)
(blink-cursor-mode -1)
(global-subword-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                dired-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Alias
(defalias 'yes-or-no-p 'y-or-n-p)

;; Buffers / Frames
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; Straight
(setq ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq package-enable-at-startup nil
      straight-use-package-by-default t
      straight-disable-native-compile nil
      straight-check-for-modifications nil
      straight-vc-git-default-clone-depth 1
      autoload-compute-prefixes nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
			             ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(require 'use-package)
(require 'straight-x)

;; Remove Messages
(advice-add 'display-startup-echo-area-message :override #'ignore)
(setq inhibit-message nil)

;; Font
(defvar default-font-size 130)
(defvar default-variable-font-size 130)
(set-face-attribute 'default nil :font "Fantasque Sans Mono" :foundry "PfEd" :slant 'normal :weight 'normal :width 'normal :height 130)
(set-face-attribute 'fixed-pitch nil :font "Fantasque Sans Mono" :height default-font-size)
(set-face-attribute 'variable-pitch nil :font "Fantasque Sans Mono" :height default-variable-font-size :weight 'regular)

;; Faces / Theme
;; (set-foreground-color "#c5c8c6")
;; (set-background-color "#1d1f21")

(setq custom-safe-themes t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "IndianRed3"))))
 ;; '(mode-line ((t (:underline (:line-width 1)))))
 '(vertico-current ((t (:background "light blue")))))
(setq x-underline-at-descent-line t)

;;;; Packages
(require 'server nil t)
(use-package server
  :straight t
  :demand t
  :if window-system
  :init
  (when (not (server-running-p server-name))
    (server-start)))

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents  . 10)
                          (bookmarks . 5)))
  (setq dashboard-banner-logo-title "")
  (setq dashboard-set-file-icons t))

(use-package tree-sitter
  :straight (tree-sitter :branch "master")
  :hook ((emacs-startup . global-tree-sitter-mode)
         (tree-sitter-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :straight (tree-sitter-langs :branch "master")
  :after tree-sitter
  :demand t
  :config
  (push '(typescript-react-mode . typescript)
        tree-sitter-major-mode-language-alist)
  (push '(web-mode . html)
        tree-sitter-major-mode-language-alist))

(use-package window
  :straight (window :type built-in)
  :config
  (setq display-buffer-alist
        `(("\\*\\(scheme\\|info\\|eshell\\|vterm\\)\\*"
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
           "\\*eshell\\*"
           "\\*vterm\\*"
           "\\*info\\*"
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

(use-package vertico
  :straight (vertico :repo "minad/vertico"
                     :branch "main")
  :config
  (setq
   vertico-count 7
   vertico-cycle t
   vertico-resize nil)
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)
  :custom-face
  (vertico-current ((t (:background "light blue"))))
  :init
  (vertico-mode))

(use-package corfu
  :straight (corfu :repo "minad/corfu" :branch "main")
  :bind (:map corfu-map
              ("<tab>" . corfu-insert))
  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-count 10
        corfu-preview-current nil
        corfu-auto-prefix 3
        corfu-auto-delay 0.01
        corfu-quit-at-boundary t
        corfu-quit-no-match t)
  :init
  (corfu-global-mode))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package prescient
  :straight t
  :custom
  (prescient-history-length 1000)
  :init
  (setq prescient-persist-mode t))

(use-package emacs
  :straight (emacs :type built-in)
  :init
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq enable-recursive-minibuffers t))

(use-package embark
  :straight t
  :after minibuffer
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (define-key embark-file-map (kbd "o") (my/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))
  (define-key embark-file-map (kbd "S") 'sudo-find-file)
  :bind (:map minibuffer-local-map
              ("C-c C-o" . embark-export))
  :bind*
  ("C-o" . embark-act)
  ("C-h h" . embark-bindings))

(eval-when-compile
  (defmacro my/embark-ace-action (fn)
    `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (require 'ace-window)
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn)))))))

(defun sudo-file-path (file)
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let (user (file-remote-p file 'user))
                              (concat user "@" host)
                            host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))

;;;###autoload
(defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (sudo-file-path file)))

;;;###autoload
(defun sudo-this-file ()
  "Open the current file as root."
  (interactive)
  (find-file
   (sudo-file-path
    (or buffer-file-name
        (when (or (derived-mode-p 'dired-mode)
                  (derived-mode-p 'wdired-mode))
          default-directory)))))

;;;###autoload
(defun sudo-save-buffer ()
  "Save this file as root."
  (interactive)
  (let ((file (sudo-file-path buffer-file-name)))
    (if-let (buffer (find-file-noselect file))
        (let ((origin (current-buffer)))
          (copy-to-buffer buffer (point-min) (point-max))
          (unwind-protect
              (with-current-buffer buffer
                (save-buffer))
            (unless (eq origin buffer)
              (kill-buffer buffer))
            (with-current-buffer origin
              (revert-buffer t t))))
      (user-error "Unable to open %S" file))))

(use-package embark-consult
  :straight '(embark-consult :host github
                             :repo "oantolin/embark"
                             :files ("embark-consult.el"))
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package consult
  :straight t
  :demand t
  :after project
  :bind (("C-s" . consult-line)
         ("C-M-m" . consult-imenu)
         ("C-M-S-m" . consult-imenu-multi)
         ("C-M-s" . consult-multi-occur)
         ("C-M-l" . consult-outline)
         ("M-g M-g" . consult-goto-line)
         ("C-c f" . consult-flymake)
         ("C-x M-f" . consult-recent-file)
         ([remap popup-kill-ring] . consult-yank-from-kill-ring)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-project-root-function (lambda () "Return current project root"
                                        (project-root (project-current))))
  (setq consult-narrow-key "<")
  (setq consult-preview-key (kbd "M-."))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (consult-line-start-from-top nil)
  (consult-line-point-placement 'match-end)
  (fset 'multi-occur #'consult-multi-occur)
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple))

(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-map
         ("C-x j" . consult-dir-jump-file)))

(use-package better-jumper
  :straight t
  :bind
  ("C-c h g" . consult-better-jumper)
  ("C-c h r" . better-jumper-set-jump)
  ("C-c h f" . better-jumper-jump-forward)
  ("C-c h b" . better-jumper-jump-backward)
  :init
  (better-jumper-mode +1))

(defun consult-better-jumper--candidates ()
  (let ((markers (better-jumper--get-marker-table)))
    (mapcar
     (pcase-lambda (`(,_path ,_pt ,key))
       (when-let ((marker (gethash key markers)))
         (save-excursion
           (goto-char marker)
           (consult--location-candidate
            (consult--line-with-cursor marker)
            marker (line-number-at-pos)))))
     (ring-elements (better-jumper--get-jump-list)))))

(defun consult-better-jumper ()
  "Browse better-jumper jump points with consult"
  (interactive)
  (consult--read
   (consult-better-jumper--candidates)
   :prompt "Jump to: "
   :annotate (consult--line-prefix)
   :category 'consult-location
   :sort nil
   :require-match t
   :lookup #'consult--lookup-location
   :history '(:input consult--line-history)
   :add-history (thing-at-point 'symbol)
   :state (consult--jump-state)))

(use-package affe
  :straight t
  :defer 2
  :config
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless--highlight))

(use-package marginalia
  :straight t
  :after vertico
  :init
  (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

(use-package cape
  :straight t
  :defer 2
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package lsp-mode
  :straight t
  :custom
  (lsp-completion-provider :none)
  :preface
  (defun my/lsp-format-buffer ()
    (interactive)
    ;; (lsp-format-buffer)
    (delete-trailing-whitespace))
  :bind (:map lsp-mode-map
              ("C-c o d" . lsp-describe-thing-at-point)
              ("C-c o f" . my/lsp-format-buffer)
              ("C-c o a" . lsp-execute-code-action)
              ("C-c o r" . lsp-find-references)
              ("C-c o g" . lsp-find-definition))
  :custom
  (lsp-modeline-diagnostics-enable nil)
  (lsp-enable-folding nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-signature-render-documentation nil)
  (lsp-completion-show-detail nil)
  (lsp-eldoc-render-all nil)
  (lsp-enable-snippet t)
  (lsp-eldoc-enable-hover nil)
  (lsp-document-sync-method nil)
  (lsp-signature-auto-activate nil)
  (lsp-print-performance t)
  (lsp-before-save-edits nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-signature-render-documentation t)
  :init
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)

  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
  ;; :config
  ;; (setq lsp-disabled-clients '(eslint vls))
  ;; (setq lsp-enabled-clients '(lsp-volar))
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

;; (use-package lsp-volar
;;   :straight (lsp-volar :host github :repo "jadestrong/lsp-volar"))

(use-package wgrep
  :defer 2
  :straight t
  :config
  (defun custom-wgrep-apply-save ()
    "Apply the edits and save the buffers"
    (interactive)
    (wgrep-finish-edit)
    (wgrep-save-all-buffers))

  (setq wgrep-change-readonly-file t)
  :bind (:map wgrep-mode-map
              ("C-x C-s" . custom-wgrep-apply-save)))

(use-package php-mode
  :straight t
  :mode "\\.php\\'"
  :hook (php-mode . lsp-deferred))

;; (use-package restclient
;;   :straight t
;;   :defer 10)

(use-package helpful
  :straight t
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package info-colors
  :straight t
  :init
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

(use-package ace-window
  :straight t
  :config
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-x o" . ace-window)
         ("M-o" . other-window)
         ("C-x 0" . ace-delete-window)
         ("C-x O" . ace-swap-window)
         ("C-x M-0" . delete-other-windows)))

(use-package project
  :straight (project :type built-in)
  :init
  (global-set-key (kbd "C-c p") project-prefix-map)
  (cl-defgeneric project-root (project) (car project))
  (setq project-switch-commands
        '((?f "Find file" project-find-file)
          (?g "Find regexp" project-find-regexp)
          (?d "Dired" project-dired)
          (?b "Buffer" project-switch-to-buffer)
          (?r "Query replace" project-query-replace-regexp)
          (?v "VC-Dir" project-vc-dir)
          (?k "Kill buffers" project-kill-buffers)
          (?! "Shell command" project-shell-command)
          (?e "Eshell" consult-recent-file)))
  :bind*
  ("C-c p s r" . consult-ripgrep))

(use-package tab-bar
  :straight (tab-bar :type built-in)
  :hook (after-init . (lambda ()
                        (doom-modeline-def-segment workspace-name
                          (when doom-modeline-workspace-name
                            (when-let
                                ((name (cond
                                        (t
                                         (let* ((current-tab (tab-bar--current-tab))
                                                (tab-index (tab-bar--current-tab-index))
                                                (explicit-name (alist-get 'name current-tab))
                                                (tab-name (alist-get 'name current-tab)))
                                           (if explicit-name tab-name (+ 1 tab-index)))))))
                              (propertize (format " %s " name) 'face
                                          (if (doom-modeline--active)
                                              'doom-modeline-buffer-major-mode
                                            'mode-line-inactive)))))))
  :config
  (tab-bar-history-mode 1)
  (setq  tab-bar-close-last-tab-choice 'tab-bar-mode-disable
         tab-bar-show                   nil
         tab-bar-new-tab-choice        'ibuffer
         tab-bar-tab-name-truncated-max 14)
  :bind-keymap ("C-c t" . tab-prefix-map)
  :bind
  ("C-c C-'" . tab-bar-switch-to-recent-tab)
  (:map tab-prefix-map
        ("n" . tab-bar-switch-to-next-tab)
        ("p" . tab-bar-switch-to-prev-tab)
        ("N" . tab-bar-history-forward)
        ("P" . tab-bar-history-back))
  :bind*
  ("C-x C-p" . tab-switch)
  :init
  (tab-bar-new-tab)
  (tab-bar-mode -1))

(use-package tab-bar-echo-area
  :straight t
  :after tab-bar
  :init
  (defvar tab-bar-format nil "Format for tab-bar-echo-area-mode")
  :config
  (tab-bar-echo-area-mode 1))

(use-package avy
  :straight t
  :config
  (setq avy-timeout-seconds 0.35)
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?l ?\;
                      ?v ?b ?n ?. ?, ?/ ?u ?p ?e
                      ?c ?q ?2 ?3 ?'))
  (setq avy-dispatch-alist '((?k . avy-action-kill-move)
                             (?K . avy-action-kill-stay)
                             (?x . avy-action-copy-whole-line)
                             (?X . avy-action-kill-whole-line)
                             (?t . avy-action-teleport)

                             (?m . avy-action-mark)
                             (?M . avy-action-mark-to-char)
                             (?w . avy-action-copy)
                             (?y . avy-action-yank)
                             (?Y . avy-action-yank-line)
                             (?i . avy-action-ispell)
                             (?z . avy-action-zap-to-char)
                             (?o . avy-action-embark)))
  :custom
  (avy-single-candidate-jump nil)
  :bind*
  ("C-j" . avy-goto-char-timer)
  ("M-m" . avy-goto-word-0))

(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(defun avy-action-teleport-whole-line (pt)
  (avy-action-kill-whole-line pt)
  (save-excursion (yank)) t)

(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(defun avy-action-embark (pt)
  (save-excursion
    (goto-char pt)
    (embark-act))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(use-package embrace
  :straight t
  :bind
  ("M-s a" . embrace-add)
  ("M-s c" . embrace-change)
  ("M-s d" . embrace-delete))

(use-package expand-region
  :straight t
  :bind (("C-}" . er/expand-region)
         ("C-M-}" . er/mark-outside-pairs)
         ("C-{" . er/select-text-in-delims)))

(defun er/select-text-in-delims ()
  (interactive)
  (let ( $skipChars $p1 )
    (setq $skipChars "^\"'`<>(){}[]‹›«»")
    (skip-chars-backward $skipChars)
    (setq $p1 (point))
    (skip-chars-forward $skipChars)
    (set-mark $p1)))

;; (defun er/change-in-sexp (arg char)
;;   (interactive "p\ncZap to char: ")
;;   (re-search-backward (char-to-string char))
;;   (sp-change-inner))

(use-package no-littering
  :straight t)

(use-package devdocs
  :defer 2
  :straight t
  :config
  (defun my/devdocs-lookup ()
    (interactive)
    (devdocs-lookup nil (thing-at-point 'word 'no-properties)))
  :bind ("C-c o D" . my/devdocs-lookup))

(add-hook 'web-mode-hook
          (lambda () (setq-local devdocs-current-docs '("vue~3"))))
(add-hook 'python-mode-hook
          (lambda () (setq-local devdocs-current-docs '("django_rest_framework" "django~3.2"))))
(add-hook 'php-mode-hook
          (lambda () (setq-local devdocs-current-docs '("laravel~8"))))

(defun org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


  (defun org-archive-done-tasks ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "/DONE" 'tree))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Fantasque Sans Mono" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;; Custom Org
(defun my/org-new-project ()
  (interactive)
  (insert "* " (read-string "Enter Project Name:"))
  (insert " [%]")
  (save-excursion
    (insert (format (concat "\n"
                            ":PROPERTIES:\n"
                            ":CREATED:      %s\n"
                            ":COOKIE_DATA:  todo recursive\n"
                            ":ID:           %s\n"
                            ":END:\n")
                    (format-time-string (org-time-stamp-format t t))
                    (substring (shell-command-to-string "uuidgen") 0 -1))))
  (org-backward-heading-same-level 0)
  (org-toggle-tag "project" 'on)
  (org-next-visible-heading 1))

(defun my/org-new-todo-header ()
  (insert "TODO ")
  (save-excursion
    (insert (format (concat "\n"
                            "DEADLINE:   %s SCHEDULED:  %s\n"
                            ":PROPERTIES:\n"
                            ":CREATED:    %s\n"
                            ":ID:         %s\n"
                            ":END:\n")
                    (format-time-string "[%Y-%m-%d %a %H:%M]" (org-read-date t 'to-time nil))
                    (format-time-string "[%Y-%m-%d %a %H:%M]" (org-read-date t 'to-time nil))
                    (format-time-string (org-time-stamp-format t t))
                    (substring (shell-command-to-string "uuidgen") 0 -1)))))

(defun my/org-new-inline-heading ()
  (interactive)
  (org-insert-heading)
  (my/org-new-todo-header))

(defun my/org-new-sub-heading ()
  (interactive)
  (org-insert-subheading (org-current-level))
  (my/org-new-todo-header))

(use-package org
  :straight t
  :commands (org-capture org-agenda)
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.75))
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)

  (setq org-agenda-files (directory-files-recursively "~/.config/emacs/org/" "\\.org$"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "NOTE(n)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
        '(("Archive.org" :maxlevel . 1)
          ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
          (:endgroup)
          ("@errand" . ?E)
          ("@home" . ?H)
          ("@work" . ?W)
          ("agenda" . ?a)
          ("batch" . ?b)
          ("note" . ?n)
          ("project" . ?p)
          ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("W" "Work Tasks" tags-todo "+work-email")

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/.config/emacs/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "~/.config/emacs/org/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)
          ("jm" "Meeting" entry
           (file+olp+datetree "~/.config/emacs/org/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree "~/.config/emacs/org/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (org-font-setup)
  :bind (:map org-mode-map
              ("C-c o p" . my/org-new-project)
              ("C-c o i" . my/org-new-inline-heading)
              ("C-c o s" . my/org-new-sub-heading)))

(use-package ts
  :straight t)

(use-package htmlize
  :defer 2
  :straight (htmlize :host github :repo "hniksic/emacs-htmlize")
  :config
  (setq org-html-htmlize-output-type 'css))

;; (use-package org-gantt-mode
;;   :straight (org-gantt-mode :host gitlab :repo "joukeHijlkema/org-gantt")
;;   :init
;;   (defvar og-Cols '((strokeColor  . "#000000")
;;                     (dayColor     . "#d7d7d7")
;;                     (bgTitleBlock . "#ffffff")
;;                     (bgGridHead   . "#ffffff")
;;                     (bgTaskEven   . "#ffffff")
;;                     (bgTaskOdd    . "#ffffff")
;;                     (bgTaskLevel1 . "#bfc0c4")
;;                     (taskDone     . "#4fe42f")
;;                     (taskDuration . "#f0dd60")
;;                     (fillTaskLevel1 . "#000000")
;;                     (bgTask       . "#77baff")
;;                     (bgTaskP      . "white")
;;                     (bgKP         . "#77baff")
;;                     (bgKPdone     . "#4fe42f")
;;                     (stLink       . "#fe6060"))
;;     "default gantt colors")
;;   (setq org-gantt-mode t))

(use-package toc-org
  :straight t
  :defer 5
  :init
  (add-hook 'org-mode-hook 'toc-org-mode))

(use-package org-superstar
  :straight (org-superstar-mode :host github :repo "integral-dw/org-superstar-mode")
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-todo-bullet-alist
   '(("TODO" . 9744)
     ("DONE" . 9745)))
  (org-superstar-cycle-headline-bullets t)
  (org-hide-leading-stars t)
  (org-superstar-special-todo-items t))

(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :straight t
  :hook (org-mode . org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

(use-package org-roam
  :defer 3
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/.config/emacs/org/Notes/Roam")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n t" . org-roam-dailies-capture-today)
         ("C-c n r" . org-roam-dailies-capture-tomorrow)
         ("C-c n y" . org-roam-dailies-capture-yesterday)
         ("C-c n g t" . org-roam-dailies-goto-today)
         ("C-c n g r" . org-roam-dailies-goto-tomorrow)
         ("C-c n g y" . org-roam-dailies-goto-yesterday))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

(defun my/map-line-to-status-char (line)
  (cond ((string-match "^?\\? " line) "?")))

(defun my/get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
        (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

(defun my/pwd-shorten-dirs (pwd)
  (let ((p-lst (split-string pwd "/")))
    (if (> (length p-lst) 2)
        (concat
         (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                    (substring elm 0 0)))
                    (butlast p-lst 2)
                    "/")
         "/"
         (mapconcat (lambda (elm) elm)
                    (last p-lst 2)
                    "/"))
      pwd)))

(defun my/eshell-prompt ()
  (concat
   "\n"
   (propertize (user-login-name) 'face `(:foreground "#8f0075"))
   (propertize " @ " 'face `(:foreground "#2544bb"))
   (propertize (my/pwd-shorten-dirs (my/get-prompt-path)) 'face `(:foreground "#145c33"))
   (propertize " #" 'face `(:foreground "#70480f"))
   (propertize " " 'face `(:foreground "white"))))

(defun eshell-configure ()
  (use-package xterm-color
    :straight t)

  (push 'eshell-tramp eshell-modules-list)
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (add-hook 'eshell-pre-command-hook
            (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
            (lambda () (setenv "TERM" "dumb")))

  (define-key eshell-mode-map (kbd "<tab>") 'capf-autosuggest-forward-word)
  (define-key eshell-mode-map (kbd "C-r") 'consult-history)
  (define-key eshell-mode-map (kbd "C-a") 'eshell-bol)
  (define-key eshell-mode-map (kbd "C-l") (lambda () (interactive) (eshell/clear 1) (eshell-send-input)))
  (eshell-hist-initialize)
  (setenv "PAGER" "cat")

  (setq eshell-prompt-function 'my/eshell-prompt
        eshell-prompt-regexp "[a-zA-z]+ @ [^#$\n]+ # "
        eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-highlight-prompt t
        eshell-scroll-to-bottom-on-input t
        eshell-prefer-lisp-functions nil
        comint-prompt-read-only t)
  (generate-new-buffer eshell-buffer-name))

(use-package eshell
  :straight (eshell :type built-in)
  :hook (eshell-first-time-mode . eshell-configure)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh"))))

(use-package vterm
  :straight t
  :commands vterm-mode
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 5000)
  (defun set-no-process-query-on-exit ()
    (let ((proc (get-buffer-process (current-buffer))))
      (when (processp proc)
        (set-process-query-on-exit-flag proc nil))))
  (add-hook 'vterm-mode-hook 'set-no-process-query-on-exit))

(use-package capf-autosuggest
  :straight (capf-autosuggest :host github :repo "emacs-straight/capf-autosuggest")
  :hook ((eshell-mode comint-mode) . capf-autosuggest-mode))

(use-package eshell-syntax-highlighting
  :straight t
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package tramp
  :straight (tramp :type built-in)
  :defer 5
  :custom
  (tramp-default-method "ssh")
  :config
  (setq tramp-verbose 1)
  (put 'temporary-file-directory 'standard-value '("/tmp"))
  (setq tramp-auto-save-directory "~/.cache/emacs/backups"
        tramp-persistency-file-name "~/.config/emacs/data/tramp"))

(use-package direnv
  :straight t
  :config
  (advice-add 'lsp :before (lambda (&optional n) (direnv-update-environment)))
  (direnv-mode))

(use-package dumb-jump
  :straight t
  :defer 5
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package eldoc
  :straight (eldoc :type built-in)
  :custom
  (eldoc-idle-delay 0)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-display-truncation-message nil))

(use-package php-mode
  :straight t
  :mode "\\.php\\'"
  :hook (php-mode . lsp-deferred))

(use-package typescript-mode
  :straight t
  :mode
  ("\\.ts\\'"
   "\\.js\\'")
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package pip-requirements
  :straight t
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

(use-package python-mode
  :straight t
  :hook (python-mode . lsp-deferred)
  :bind (:map python-mode-map
              ([remap lsp-format-buffer] . python-black-buffer))
  :config
  (setq python-shell-interpreter "python3"))

;; Elpy rebinds delete for some reason
(add-hook 'python-mode-hook
          (lambda()
            (local-unset-key (kbd "DEL"))))

(use-package pyimport
  :straight t
  :after python-mode)

(use-package pyvenv
  :straight t
  :defer 5
  :after python
  :config
  (setq pyvenv-menu t))

(use-package python-black
  :straight t
  :after python)

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'"
  :hook (nix-mode . lsp-deferred))

(use-package web-mode
  :straight t
  :mode ("\\.vue\\'")
  :hook (web-mode . lsp-deferred)
  :config
  (setq web-mode-enable-html-entities-fontification t
        web-mode-auto-close-style 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-style-padding 0
        web-mode-script-padding 0
        web-mode-enable-auto-quoting nil
        web-mode-enable-auto-pairing t)
  (setf (alist-get "javascript" web-mode-comment-formats nil nil #'equal)
        "//"))

;; (use-package vue-mode
;;   :mode ("\\.vue\\'")
;;   :hook (vue-mode . lsp-deferred))

(use-package css-mode
  :straight t
  :mode ("\\.css\\'"))

(use-package haskell-mode
  :straight t
  :mode ("\\.hs\\'")
  :hook (haskell-mode . lsp-deferred)
  :config
  (setq haskell-process-type 'cabal-repl))

;; finds executable and some additional compiler settings
(use-package lsp-haskell
  :straight t
  :after lsp-mode
  :hook (haskell-mode . lsp-deferred)
  :custom
  (lsp-haskell-server-path "haskell-language-server"))

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(use-package emacs-lisp-mode
  :straight (emacs-lisp-mode :type built-in)
  :hook (lisp-mode . emacs-lisp-mode))

(use-package scheme-mode
  :straight (scheme-mode :type built-in)
  :mode ("\\.sld\\'"))

(use-package racket-mode
  :straight t
  :mode ("\\.rkt\\'"))

(use-package go-mode
  :straight t
  :mode ("\\.go\\'")
  :hook(go-mode . lsp-deferred)
  :config
  (setq lsp-gopls-server-path "/home/ryan/go/bin/gopls"))

(use-package rustic
  :straight t
  :mode ("\\.rs$" . rustic-mode)
  :config
  (setq rustic-lsp-server 'rls)
  (setq rustic-lsp-server 'rustfmt)
  (setq rustic-lsp-client 'lsp-mode)
  (setq rustic-indent-method-chain t))

(add-hook 'rustic-mode-hook #'rustic-lsp-mode-setup)

(use-package erlang
  :straight t
  :mode ("\\.erl$" . erlang-mode)
  :hook (erlang-mode . lsp-deferred))

(use-package tuareg
  :straight t
  :mode ("\\.ml$" . tuareg-mode)
  :hook (tuareg-mode . lsp-deferred))

(use-package latex
  :defer 5
  :straight (latex :type built-in)
  :after tex
  :mode ("\\.tex\\'" . LaTeX-mode))

;; (use-package auctex
;;   :straight (auctex :type built-in))

(use-package cdlatex
  :straight (cdlatex :type built-in)
  :defer 5
  :after latex
  :hook (LaTeX-mode . turn-on-cdlatex))

(use-package anzu
  :straight t
  :bind
  ([remap query-replace] . anzu-query-replace)
  ([remap query-replace-regexp] . anzu-query-replace-regexp)
  :config
  (global-anzu-mode +1))

(use-package rg
  :defer 3
  :straight t)

(use-package magit
  :straight t
  :commands (magit-status magit-get-current-branch)
  :bind ("C-c g" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :straight t
  :after magit)

(use-package evil-nerd-commenter
  :straight t
  :bind ("C-;" . evilnc-comment-or-uncomment-lines))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (setq show-paren-mode 1))

;; Colors for # colors
(use-package rainbow-mode
  :straight t
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         typescript-mode))

(use-package yasnippet
  :straight t
  :defer 2
  :init
  (yas-global-mode 1)
  :config
  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (defun tab-complete-or-next-field ()
    (interactive)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if corfu--candidates
            (progn
              (corfu-insert)
              (yas-next-field))
          (yas-next-field))))
  (yas-reload-all)
  :bind (:map yas-keymap
              ("<tab>" . tab-complete-or-next-field)))

(use-package flymake
  :straight (flymake :type built-in)
  :defer 3
  :init
  (setq-default flymake-diagnostic-functions nil)
  (with-eval-after-load 'flymake-proc
    (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))
  :config
  (setq flymake-start-on-flymake-mode t
        flymake-start-on-save-buffer t))

(use-package flyspell
  :straight (flyspell :type built-in)
  :hook (text-mode . flyspell-mode))

(use-package smartparens
  :straight t
  :hook (prog-mode . smartparens-mode)
  (text-mode . smartparens-mode)
  :config
  (sp-local-pair '(emacs-lisp-mode scheme-mode) "'" "'" :actions nil))

(use-package paren
  :straight t
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(use-package dired
  :straight (dired :type built-in)
  :hook ((dired-mode . hl-line-mode)
         (dired-mode . toggle-truncate-lines))
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("q" . dired-up-directory))
  :custom
  ((dired-listing-switches "-AGFhlv --group-directories-first")
   (dired-recursive-copies t))
  :config
  (setf dired-kill-when-opening-new-dired-buffer t)
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always
        delete-by-moving-to-trash t))

(use-package all-the-icons-dired
  :straight t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package diredfl
  :straight t
  :hook (dired-mode . diredfl-mode)
  :init
  (setq diredfl-ignore-compressed-flag nil)
  (diredfl-global-mode 1))

(setq-default tab-width 2
              indent-tabs-mode nil)

(use-package multiple-cursors
  :straight t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-<" . mc/mark-next-like-this)
         ("C->" . mc/mark-previous-like-this)
         ("C-c m a" . mc/mark-all-like-this)))

(defun my/beginning-of-line ()
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
	  (beginning-of-visual-line)))

(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun shift-right (count)
  (interactive "p")
  (shift-text count))

(defun shift-left (count)
  (interactive "p")
  (shift-text (- count)))

;; Smarter C-Backspace control
(defun my/backward-kill-word ()
  (interactive)
  (let* ((cp (point))
         (backword)
         (end)
         (space-pos)
         (backword-char (if (bobp)
                            ""
                          (buffer-substring cp (- cp 1)))))
    (if (equal (length backword-char) (string-width backword-char))
        (progn
          (save-excursion
            (setq backword (buffer-substring (point) (progn (forward-word -1) (point)))))
          (save-excursion
            (message (thing-at-point 'no-properties))
            (when (and backword
                       (string-match-p " " backword))
              (setq space-pos (ignore-errors (search-backward " ")))))
          (save-excursion
            (let* ((pos (ignore-errors (search-backward-regexp "\n")))
                   (substr (when pos (buffer-substring pos cp))))
              (when (or (and substr (string-blank-p (string-trim substr)))
                        (string-match-p "\n" backword))
                (setq end pos))))
          (if end
              (kill-region cp end)
            (if space-pos
                (kill-region cp space-pos)
              (backward-kill-word 1))))
      (kill-region cp (- cp 1)))))

(defun my/op-thing-at-point (op thing)
  "Get the start and end bounds of a type of thing at point."
  (superword-mode 1)
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (funcall op (car bounds) (cdr bounds))
      (error "No %s at point" thing)))
  (superword-mode -1)
  (global-subword-mode 1))

;; General binds
(global-set-key (kbd "C-c w") (lambda () (interactive) (my/op-thing-at-point 'copy-region-as-kill 'word)))
;; (global-set-key (kbd "C-c i") #'er/change-in-sexp)
(global-set-key (kbd "C-x C-b") #'switch-to-buffer)
(global-set-key (kbd "C-c C-v") (lambda () (interactive) (switch-to-buffer nil)))
(define-key prog-mode-map (kbd "C-a") #'my/beginning-of-line)
(global-set-key (kbd "M-]") #'shift-right)
(global-set-key (kbd "M-[") #'shift-left)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-d") (lambda () (interactive) (my/op-thing-at-point 'kill-region 'word)))
(global-set-key (kbd "C-M-<backspace>") #'backward-kill-sexp)
(global-set-key (kbd "C-M-<return>") #'vterm)
(global-set-key (kbd "C-S-k") #'kill-whole-line)
(global-set-key (kbd "C-x c f") (lambda () (interactive) (find-file "~/.config/emacs/init.el")))
(global-set-key (kbd "C-x c e")  #'dashboard-refresh-buffer)
(global-set-key (kbd "C-c o g")  #'xref-find-definitions)
(global-set-key (kbd "C-/")  #'undo-only)
(global-set-key (kbd "C-?")  #'undo-redo)
;; (global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

(bind-key* "C-<backspace>" #'my/backward-kill-word)



;; unbind annoying keybinds
(global-unset-key  (kbd "C-x C-n"))
(global-unset-key  (kbd "M-`"))
(global-unset-key  (kbd "C-z"))
(global-unset-key  (kbd "C-x C-z"))
;; ignore command
(global-set-key [remap org-cycle-agenda-files] 'ignore)

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position)))))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Line Copied")
     (list (line-beginning-position) (line-end-position)))))

(defadvice kill-line (before kill-line-autoreindent activate)
  "Kill excess whitespace when joining lines.
If the next line is joined to the current line, kill the extra indent whitespace in front of the next line."
  (when (and (eolp) (not (bolp)))
    (save-excursion
      (forward-char 1)
      (just-one-space 1))))

(defun pils-follow (&rest _arg)
  "Advice to follow a function which spawn a window."
  (other-window 1))

(advice-add 'split-window-below :after #'pils-follow)
(advice-add 'split-window-right :after #'pils-follow)

;; Save position in better jumper before jumping anywhere
(advice-add 'find-function :before 'better-jumper-set-jump)
(advice-add 'consult-ripgrep :before 'better-jumper-set-jump)
(advice-add 'consult-line :before 'better-jumper-set-jump)

;; Load theme
(use-package modus-themes
  ;; :straight (modus-themes :host github :repo "protesilaos/modus-themes")
  :straight t
  :init
  (setq  modus-themes-intense-hl-line t
         modus-themes-org-blocks 'grayscale
         modus-themes-scale-headings t
         modus-themes-section-headings nil
         modus-themes-variable-pitch-headings nil
         modus-themes-intense-paren-match t
         modus-themes-diffs 'desaturated
         modus-themes-syntax '(alt-syntax green-strings yellow-comments)
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
        '(
          (bg-main . "#F5F5F5")
          (bg-dim . "#F8F8F8")
          (bg-alt . "#E8E8E8")
          ;; (blue-alt-other . "#0f3d8c")
          ;; (blue-alt . "#2544bb")
          ;; (magenta-alt-other . "#55348e")
          ;; (magenta-alt . "#752f50")
          ;; (magenta-intense . "#8f0075")
          ))
  (load-theme 'modus-operandi))

(use-package savehist
  :defer 2
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (locate-user-emacs-file "var/savehist.el"))
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t))

(use-package recentf
  :straight (:type built-in)
  :defer 2
  :config
  (setq recentf-max-saved-items 100)
  (recentf-mode 1))

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
  :custom ((doom-modeline-height 10))
  :config
  (setq doom-modeline-buffer-modification-icon nil)
  ;; (setq doom-modeline-buffer-file-name-style 'file-name)
  (setq doom-modeline-hud t)
  :init
  (doom-modeline-mode +1))
