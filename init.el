;; Function to automatically generate a .el for our .org configuration files
(defun org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org-babel-tangle-config)))

;; native comp
(setq comp-deferred-compilation t)
(setq comp-speed 3)
(setq comp-async-report-warnings-errors nil)
(setq warning-minimum-level :error)
(setq package-native-compile 1)

;; Stop the native comp warnings
(defvar grep-find-ignored-directories nil)
(defvar grep-find-ignored-files nil)
(defvar ido-context-switch-command nil)
(defvar ido-cur-item nil)
(defvar ido-cur-list nil)
(defvar ido-default-item nil)
(defvar inherit-input-method nil)
(defvar oauth--token-data nil)
(defvar tls-checktrust nil)
(defvar tls-program nil)
(defvar url-callback-arguments nil)
(defvar url-callback-function nil)
(defvar url-http-extra-headers nil)

;; Set default font size values
(defvar default-font-size 140)
(defvar default-variable-font-size 140)

;; Set default transparency values
(defvar frame-transparency '(100 . 100))

;; Default to utf-8
(setq default-buffer-file-coding-system 'utf-8-unix
      buffer-file-coding-system 'utf-8-unix)

(push "node_modules/" completion-ignored-extensions)
(push "__pycache__/" completion-ignored-extensions)

;; Syntax highlight for all buffers
(global-font-lock-mode t)

;; Dont save duplicate variables in kill ring
(setq kill-do-not-save-duplicates t)

;; When using gui confirm before closing
(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

;; Weird
(setq system-uses-terminfo nil)

;; Set exec paths for npm packages on nix
(add-to-list 'exec-path "/root/.npm/bin")

(setq
 ;; Dont show these file types in recent files
 recentf-exclude (list (rx
                        "COMMIT_EDITMSG"
                        (and (or "/TAGS"
                                 "/GTAGS"
                                 "/GRAGS"
                                 "/GPATH"
                                 ".mkv"
                                 ".avi"
                                 (and ".mp" (any "3" "4"))
                                 (and ".doc" (? "x"))
                                 ".sub"
                                 ".srt"
                                 ".ass"
                                 ".elc"
                                 (and "tmp." (+ (not (any "/" "\\")))))
                             eol))))

;; General Defaults
(setq delete-old-versions t
      delete-by-moving-to-trash t
      enable-recursive-minibuffers t)

(blink-cursor-mode -1)
(setq uniquify-buffer-name-style 'forward)

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Remove startup message
(advice-add 'display-startup-echo-area-message :override #'ignore)

(setq gc-cons-threshold (* 50 1000 1000))

(defun display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

;; Call the function
(add-hook 'emacs-startup-hook #'display-startup-time)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms (incase I ever use emacs on windows)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure nil
      straight-disable-native-compile nil
      straight-use-package-by-default nil)

;; Bootstrap straight.el
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

;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default (not (eq system-type 'gnu/linux)))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

;; (use-package auto-package-update
;;   :straight t
;;   :custom
;;   (auto-package-update-interval 7)
;;   (auto-package-update-prompt-before-update t)
;;   (auto-package-update-hide-results t)
;;   :config
;;   (auto-package-update-maybe)
;;   (auto-package-update-at-time "09:00"))

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner "~/.config/emacs/gnu.png")
  (setq dashboard-items '((recents  . 10)
                          (projects . 5)
                          (bookmarks . 5)))
  (setq dashboard-banner-logo-title "")
  (setq dashboard-set-file-icons t))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)

(menu-bar-mode -1)            ; Disable the menu bar

(column-number-mode)
(global-display-line-numbers-mode t) ; Line numbers

;; y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Better scrolling
(setq scroll-conservatively 100
      scroll-preserve-screen-position t)

;; Kill server if there is one and start fresh
(require 'server nil t)
(use-package server
  :straight t
  :demand t
  :if window-system
  :init
  (when (not (server-running-p server-name))
    (server-start)))

(set-face-attribute 'default nil :font "Source Code Pro" :height default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Source Code Pro" :height default-variable-font-size :weight 'regular)

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
(load-theme 'wombat t)

(set-foreground-color "#c5c8c6")
(set-background-color "#1d1f21")

;; Custom faces
(custom-set-faces
 `(match ((t (:foreground "#72a4ff"))))
 `(persp-selected-face ((t (:foreground "light green"))))
 `(doom-modeline-buffer-major-mode ((t (:foreground "light blue"))))
 `(doom-modeline-info ((t (:foreground "pink"))))
 `(doom-modeline-unread-number ((t (:foreground "red"))))
 `(mode-line ((t (:foreground "#c5c8c6"))))
 `(org-level-4 ((t (:foreground "light blue"))))
 `(show-paren-match ((t (:background "steelblue" :foreground "light green"))))
 `(web-mode-html-tag-custom-face ((t (:foreground "#a4c460"))))
 `(web-mode-html-tag-face ((t (:foreground "#78add2"))))
 `(web-mode-html-attr-name-face ((t (:foreground "MediumPurple2"))))
 )


;; For the default theme
(custom-set-faces
 '(company-preview
   ((t (:background "#1d1f21" :foreground "white" :underline t))))
 '(company-preview-common
   ((t (:inherit company-preview))))
 '(company-tooltip
   ((t (:background "#1d1f21" :foreground "white"))))
 '(company-tooltip-selection
   ((t (:background "steelblue" :foreground "white")))))

(use-package tree-sitter-langs
         :straight t)

(use-package tree-sitter
  :straight t
  :config
  (require 'tree-sitter-langs)
  (require 'tree-sitter-debug)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package popper
  :straight t
  :after projectile
  :bind (("C-c C-." . popper-toggle-latest)
         ("C-c M-." . popper-kill-latest-popup)
         ("C-c C-/" . popper-cycle)
         ("C-c C-;" . popper-toggle-type))
  :init
  (setq popper-mode-line nil)
  (setq popper-group-function #'popper-group-by-projectile)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1))

(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

(use-package all-the-icons
  :straight t)

(use-package doom-modeline
  :straight t
  :init
  (doom-modeline-mode 1)
  (setq projectile-dynamic-mode-line nil)
  (setq doom-modeline-bar-width 3
        doom-modeline-minor-modes nil
        doom-modeline-github nil
        doom-modeline-buffer-file-name-style 'relative-from-project)
  :custom ((doom-modeline-height 15)
           (doom-modeline-project-detection 'project)))

;; Completion framework
(use-package vertico
  :straight (vertico :repo "minad/vertico"
                     :branch "main")
  :config
  (setq
   vertico-count 7
   vertico-cycle t
   vertico-resize nil)
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode))

;; Completion ordering
(use-package orderless
  :straight t
  :demand t
  :config
  (defun orderless-company-fix-face+ (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))

  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (orderless partial-completion)))))

  (with-eval-after-load 'company
    (advice-add 'company-capf--candidates :around #'orderless-company-fix-face+)))

(use-package prescient
  :straight t
  :demand t
  :custom
  (prescient-history-length 1000)
  :config
  (prescient-persist-mode +1))

(use-package savehist
  :straight (savehist :type built-in)
  :hook (after-init . savehist-mode)
  :custom
  (savehist-additional-variables
   '(kill-ring search-ring regexp-search-ring
               consult--line-history evil-ex-history
               projectile-project-command-history)))

;; Mainly for recursive minibuffers
(use-package emacs
  :straight (emacs :type built-in)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Completion actions
(use-package embark
  :straight t
  :bind (:map minibuffer-mode-map
              ("C-S-a" . embark-act)
              ("C-c C-o" . embark-export))
  :config
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

;; Additonal completion actions
(use-package embark-consult
  :straight '(embark-consult :host github
                             :repo "oantolin/embark"
                             :files ("embark-consult.el"))
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

;; Similar to counsel
(use-package consult
  :straight t
  :demand t
  :after projectile
  :bind (("C-s" . consult-line)
         ("C-M-s" . multi-occur)
         ("C-M-l" . consult-outline)
         ("M-g M-g" . consult-goto-line)
         ("C-S-c c" . consult-mark)
         ("C-x M-f" . consult-recent-file)
         ([remap popup-kill-ring] . consult-yank-from-kill-ring)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :config
  (setq consult-project-root-function #'projectile-project-root)
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (consult-line-start-from-top nil)
  (consult-line-point-placement 'match-end)
  (fset 'multi-occur #'consult-multi-occur)
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format))

(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-map
         ("C-x j" . consult-dir-jump-file)))

;; Similar to ivy rich but better
(use-package marginalia
  :straight t
  :after vertico
  :init
  (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config
  (advice-add #'marginalia--project-root :override #'projectile-project-root)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (setq marginalia-command-categories
        (append '((projectile-find-file . project-file)
                  (projectile-find-dir . project-file)
                  (projectile-switch-project . project-file)
                  (projectile-recentf . project-file)
                  (projectile-switch-to-buffer . buffer)
                  (persp-switch-to-buffer . buffer))
                marginalia-command-categories)))

(use-package wgrep
  :straight t
  :config
  (setq wgrep-change-readonly-file t)
  :bind (
         :map wgrep-mode-map
         ("C-x C-s" . custom-wgrep-apply-save)))


(defun custom-wgrep-apply-save ()
  "Apply the edits and save the buffers"
  (interactive)
  (wgrep-finish-edit)
  (wgrep-save-all-buffers))

(defun reb-query-replace (to-string)
  "Replace current RE from point with `query-replace-regexp'."
  (interactive
   (progn (barf-if-buffer-read-only)
          (list (query-replace-read-to (reb-target-binding reb-regexp)
                                       "Query replace"  t))))
  (with-current-buffer reb-target-buffer
    (query-replace-regexp (reb-target-binding reb-regexp) to-string)))

(use-package which-key
  :straight t
  :demand t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package diminish
  :straight t)

(use-package helpful
  :straight t
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package switch-window
  :straight t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" "i" "o"))
  :bind
  ([remap other-window] . switch-window))

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(use-package perspective
  :straight t
  :bind (("C-c C-'" . persp-next)
         ("C-x M-b" . persp-switch)
         ("C-x M-k" . persp-kill)
         ("C-x C-S-B" . persp-switch-to-buffer))
  :custom
  (persp-initial-frame-name "Ext")
  :config
  (unless (equal persp-mode t)
    (persp-mode)))

(add-hook 'persp-mode-hook
          (persp-switch "Main"))

(advice-add 'imalison:avy :around 'imalison:disable-keychord-around)

(use-package avy
  :straight t
  :bind (("M-s" . avy-goto-char)
         ("C-j" . avy-goto-char-2)
         ("M-m" . avy-goto-word-0))
  :custom
  (avy-single-candidate-jump nil))

(use-package expand-region
  :straight t
  :bind (("C-}" . er/expand-region)
         ("C-M-}" . er/mark-outside-pairs)
         ("C-{" . er/mark-inside-pairs)))

(use-package no-littering
  :straight t)

;; Disable auto saving and backups and symbolic link files
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(defun org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Source Code Pro" :weight 'regular :height (cdr face)))

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

(defun org-archive-done-tasks ()
(interactive)
(org-map-entries
 (lambda ()
   (org-archive-subtree)
   (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
 "/DONE" 'tree))

(add-hook 'org-mode-hook (add-hook 'after-save-hook #'org-archive-done-tasks))

(defun org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :straight t
  :pin org
  :commands (org-capture org-agenda)
  :preface
  (defun my/project-task-file ()
    (interactive)
    (find-file (concat "~/.config/emacs/org/Projects/" (projectile-project-name) ".org")))

  :hook (org-mode . org-mode-setup)
  :bind (("M-o a" . org-agenda)
         ("M-o p t" . my/project-task-file)
         ([remap org-return-and-maybe-indent] . avy-goto-char-2))
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files (directory-files-recursively "~/.config/emacs/org/" "\\.org$"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

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
          ("planning" . ?p)
          ("publish" . ?P)
          ("batch" . ?b)
          ("note" . ?n)
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

  (org-font-setup))

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
   (propertize (user-login-name) 'face `(:foreground "light green"))
   (propertize " ⟣─ " 'face `(:foreground "dark orange"))
   (propertize (my/pwd-shorten-dirs (my/get-prompt-path)) 'face `(:foreground "yellow3"))
   (propertize " λ" 'face `(:foreground "pink2"))
   (propertize "\n" 'face `(:foreground "white"))))

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

  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
  (eshell-hist-initialize)
  (setenv "PAGER" "cat")

  ;; Disable company in eshell
  (company-mode -1)
  (setq eshell-prompt-function      'my/eshell-prompt
        eshell-prompt-regexp        "^λ "
        eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-highlight-prompt t
        eshell-scroll-to-bottom-on-input t
        eshell-prefer-lisp-functions nil
        comint-prompt-read-only t)
  (setq eshell-buffer-name (concat (persp-current-name) " *eshell*"))
  (generate-new-buffer eshell-buffer-name))

(use-package eshell
  :straight t
  :hook (eshell-first-time-mode . eshell-configure)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim"))))

(add-hook 'eshell-mode-hook (lambda ()
                              (define-key eshell-mode-map (kbd "C-l") (lambda () (interactive) (eshell/clear 1)))
                              (define-key eshell-mode-map (kbd "C-c o l") #'my/eshell-copy-last-output)))

(defun my/eshell-copy-last-output ()
  (interactive)
  (eshell-mark-output)
  (avi-kill-line-save)
  (eshell-interrupt-process))

(use-package eshell-syntax-highlighting
  :straight t
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package tramp
  :defer 5
  :custom
  (tramp-default-method "ssh")
  :config
  (put 'temporary-file-directory 'standard-value '("/tmp"))
  (setq tramp-auto-save-directory "~/.cache/emacs/backups"
        tramp-persistency-file-name "~/.config/emacs/data/tramp"))

(use-package company
  :straight t
  :defer 1
  :defines company-backends
  :diminish company-mode
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :init
  (global-company-mode 1)
  (setq company-auto-commit nil
        company-minimum-prefix-length 2
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        company-dabbrev-ignore-case nil
        company-require-match 'never
        company-idle-delay 0.01
        company-dabbrev-other-buffers nil
        company-dabbrev-downcase nil))

(setq-default company-backends '(company-capf))

(defvar my/company-backend-alist
  '((text-mode (:separate company-dabbrev company-yasnippet company-ispell))
    ;;(prog-mode (company-capf company-yasnippet :with)
    (prog-mode (:separate company-yasnippet company-capf company-dabbrev-code))
    (conf-mode company-capf company-dabbrev-code company-yasnippet))
  "An alist matching modes to company backends. The backends for any mode is
        built from this.")

(defun my/set-company-backend (modes &rest backends)
  "Prepends backends (in order) to `company-backends' in modes"
  (declare (indent defun))
  (dolist (mode (list modes))
    (if (null (car backends))
        (setq my/company-backend-alist
              (delq (assq mode my/company-backend-alist)
                    my/company-backend-alist))
      (setf (alist-get mode my/company-backend-alist)
            backends))))

(defun my/company-backends ()
  (let (backends)
    (let ((mode major-mode)
          (modes (list major-mode)))
      (while (setq mode (get mode 'derived-mode-parent))
        (push mode modes))
      (dolist (mode modes)
        (dolist (backend (append (cdr (assq mode my/company-backend-alist))
                                 (default-value 'company-backends)))
          (push backend backends)))
      (delete-dups
       (append (cl-loop for (mode . backends) in my/company-backend-alist
                        if (or (eq major-mode mode)  ; major modes
                               (and (boundp mode)
                                    (symbol-value mode))) ; minor modes
                        append backends)
               (nreverse backends))))))

(add-hook 'after-change-major-mode-hook
          (defun my/company-setup-backends ()
            (interactive)
            "Set `company-backends' for the current buffer."
            (setq-local company-backends (my/company-backends))))


;; annoying when used with fuzzy searching
;; (use-package company-prescient
;;   :straight t
;;   :after (prescient company)
;;   :hook (company-mode . company-prescient-mode))

;; (defvar lsp-company-backends
;;   '(:separate company-yasnippet company-capf))

(use-package lsp-mode
  :straight t
  :after direnv
  :hook (lsp)
  :bind (:map lsp-mode-map
              ("C-c o d" . lsp-describe-thing-at-point)
              ("C-c o f" . lsp-format-buffer)
              ("C-c o a" . lsp-execute-code-action)
              ("C-c o r" . lsp-find-references)
              ("C-c o g" . lsp-find-definition))
  :config
  (setq lsp-completion-provider :none)
  ;; (add-hook 'lsp-completion-mode-hook
  ;;   (defun +lsp-init-company-backends-h ()
  ;;     (when lsp-completion-mode
  ;;       (set (make-local-variable 'company-backends)
  ;;            (cons lsp-company-backends
  ;;                  (remove lsp-company-backends
  ;;                          (remq 'company-capf company-backends)))))))
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
  (lsp-signature-render-documentation t))

;; (use-package lsp-ui
;;   :straight t
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :config
;;   (setq lsp-ui-sideline-enable nil)
;;   (setq lsp-ui-sideline-ignore-duplicate t)
;;   (setq lsp-ui-doc-enable nil)
;;   (setq lsp-ui-doc-show-with-cursor nil)
;;   (setq lsp-ui-doc-show-with-mouse nil)
;;   (setq lsp-ui-sideline-show-code-actions nil)
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package direnv
  :straight t
  :config
  (advice-add 'lsp :before (lambda (&optional n) (direnv-update-environment)))
  (direnv-mode))

;; (use-package eglot
;;   :straight t
;;   :after project
;;   :hook (eglot-connect . eglot-signal-didChangeConfiguration)
;;   :commands (eglot
;;              eglot-ensure
;;              my/eglot-mode-server
;;              my/eglot-mode-server-all)
;;   :config
;;   (add-to-list 'eglot-server-programs '(php-mode . ("intelephense" "--stdio")))
;;   (add-to-list 'eglot-server-programs '(web-mode "vls"))
;;   :init
;;   (setq eglot-sync-connect 1
;;         eglot-connect-timeout 10
;;         eglot-confirm-server-initiated-edits nil
;;         eglot-autoreconnect nil
;;         eglot-autoshutdown t
;;         eglot-send-changes-idle-time 0.5
;;         eglot-auto-display-help-buffer nil
;;         eglot-stay-out-of '(company)
;;         eglot-ignored-server-capabilites '(:documentHighlightProvider))
;;   (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend)
;;   :bind
;;   ("C-c o d" . eldoc-doc-buffer)
;;   ("C-c o f" . eglot-format-buffer)
;;   ("C-c o a" . eglot-code-actions)
;;   ("C-c o r" . xref-find-references))

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

;; Format current php buffer on save
;; (defun lsp-php-install-save-hooks ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))

;; (add-hook 'php-mode-hook #'lsp-php-install-save-hooks)

(use-package typescript-mode
  :straight t
  :mode
  ("\\.ts\\'"
   "\\.Js\\'")
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package pip-requirements
  :straight t
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

;; (use-package elpy
;; :straight t
;; :init
;; (elpy-enable)
;; (setq elpy-modules '(elpy-module-sane-defaults elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-django))
;; (setq python-shell-interpreter "python3")
;; (setq elpy-rpc-python-command "python3")
;; :config
;; (pyvenv-mode 1))

(use-package python-mode
  :straight t
  :hook (python-mode . lsp-deferred)
  :bind (:map python-mode-map
              ([remap lsp-format-buffer] . elpy-autopep8-fix-code)
              ([remap lsp-describe-thing-at-point] . elpy-doc)))

;; Elpy rebinds delete for some reason
(add-hook 'python-mode-hook
          (lambda()
            (local-unset-key (kbd "DEL"))))

(use-package pyimport
  :straight t
  :after python-mode)


(use-package pyvenv
  :straight t
  :after python)

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
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-style-padding 0)
  (setq web-mode-script-padding 0))

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

(use-package projectile
  :straight t
  :defer 10
  :diminish projectile-mode
  :config (projectile-mode)
  :bind (([remap projectile-ripgrep] . consult-ripgrep))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-switch-project-action #'projectile-dired)
  :init
  (projectile-mode 1))

(use-package rg
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
  :hook (prog-mode . rainbow-delimiters-mode))

(show-paren-mode 1)

;; Colors for # colors
(use-package rainbow-mode
  :straight t
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         typescript-mode))

(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#f66d9b"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#66c1b7"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#6574cd"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#fa7b62"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#fef691"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#ff70bf"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#fdae42"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#8f87de")))))

(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1)
  :config
  (yas-reload-all))

(use-package popup-kill-ring
  :straight t
  :bind ("M-y" . popup-kill-ring))

(use-package flymake
  :straight (flymake :type built-in)
  :diminish flymake-mode
  :commands (my/flymake-first-error
             my/flymake-last-error)
  :init
  (setq-default flymake-diagnostic-functions nil)
  (with-eval-after-load 'flymake-proc
    (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))
  :config
  (setq flymake-start-on-flymake-mode t
        flymake-start-on-save-buffer t)

  (defun my/flymake-first-error ()
    (interactive)
    (let* ((ovs (flymake--overlays :compare #'< :key #'overlay-start))
           (ov (car ovs)))
      (cond
       (ov (goto-char (overlay-start ov)))
       (t (user-error "No flymake errors in the current buffer")))))

  (defun my/flymake-last-error ()
    (interactive)
    (let* ((ovs (flymake--overlays :compare #'< :key #'overlay-start))
           (ov (car (last ovs))))
      (cond
       (ov (goto-char (overlay-start ov)))
       (t (user-error "No flymake errors in the current buffer")))))
  :bind
  (:map flymake-mode-map
        ("C-c f b" . flymake-show-diagnostics-buffer)
        ("C-c f f" . my/flymake-first-error)
        ("C-c f l" . my/flymake-last-error)
        ("C-c f n" . flymake-goto-next-error)
        ("C-c f p" . flymake-goto-prev-error)))

(use-package smartparens
  :straight t
  :hook (prog-mode . smartparens-mode)
  (text-mode . smartparens-mode))

(use-package paren
  :straight t
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(add-to-list 'load-path "~/.config/emacs/etc/modules/dired+")
(require 'dired-copy-paste)
(use-package dired
  :straight (dired :type built-in)
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("K" . dired-up-directory)
         ("C-c f" . dired-copy-paste-do-copy)
         ("C-c c f" . dired-copy-paste-do-cut)
         ("C-y" . dired-copy-paste-do-paste))
  :custom
  ((dired-listing-switches "-agho --group-directories-first")
   (dired-recursive-copies t))
  :config
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always
        delete-by-moving-to-trash t))

(use-package all-the-icons-dired
  :straight t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-rainbow
  :straight t
  :after all-the-icons-dired
  :defer 2
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(use-package multiple-cursors
  :straight t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c m a" . mc/mark-all-like-this)))

(use-package undo-tree
  :straight t
  :diminish
  :bind (("C-/" . #'undo)
         ("C-?" . #'redo))
  :custom
  (undo-tree-auto-save-history t)
  :init
  (global-undo-tree-mode +1))

(defalias 'redo 'undo-tree-redo)

(defun copy-word ()
  (interactive)
  (save-excursion
    (forward-char 1)
    (backward-word)
    (kill-word 1)
    (yank)))

(defun smart-beginning-of-line ()
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

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

(defun aborn/backward-kill-word ()
  "Customize/Smart backward-kill-word."
  (interactive)
  (let* ((cp (point))
         (backword)
         (end)
         (space-pos)
         (backword-char (if (bobp)
                            ""           ;; cursor in begin of buffer
                          (buffer-substring cp (- cp 1)))))
    (if (equal (length backword-char) (string-width backword-char))
        (progn
          (save-excursion
            (setq backword (buffer-substring (point) (progn (forward-word -1) (point)))))
          (setq ab/debug backword)
          (save-excursion
            (when (and backword          ;; when backword contains space
                       (s-contains? " " backword))
              (setq space-pos (ignore-errors (search-backward " ")))))
          (save-excursion
            (let* ((pos (ignore-errors (search-backward-regexp "\n")))
                   (substr (when pos (buffer-substring pos cp))))
              (when (or (and substr (s-blank? (s-trim substr)))
                        (s-contains? "\n" backword))
                (setq end pos))))
          (if end
              (kill-region cp end)
            (if space-pos
                (kill-region cp space-pos)
              (backward-kill-word 1))))
      (kill-region cp (- cp 1)))         ;; word is non-english word
    ))

(defun avi-kill-line-save (&optional arg)
  "Copy to the kill ring from point to the end of the current line.
With a prefix argument, copy that many lines from point. Negative
arguments copy lines backward. With zero argument, copies the
text before point to the beginning of the current line."
  (interactive "p")
  (save-excursion
    (copy-region-as-kill
     (point)
     (progn (if arg (forward-visible-line arg)
              (end-of-visible-line))
            (point)))))

(defun custom-avy-copy-line ()
  (interactive)
  (save-excursion
    (avy-goto-line)
    (back-to-indentation)
    (avi-kill-line-save)))

;; General binds
(global-set-key (kbd "C-c w") #'copy-word)
(global-set-key (kbd "C-c l") #'custom-avy-copy-line)
(global-set-key (kbd "C-x C-b") #'switch-to-buffer)
(global-set-key (kbd "C-a") #'smart-beginning-of-line)
(global-set-key (kbd "M-]") #'shift-right)
(global-set-key (kbd "M-[") #'shift-left)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key [C-backspace] #'aborn/backward-kill-word)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-M-<return>") #'eshell)
(global-set-key (kbd "C-S-k") #'kill-whole-line)
(global-set-key (kbd "C-x c f") (lambda () (interactive) (find-file "~/.config/emacs/Emacs.org")))

;; Half the distance of page down and up (does make cursor position change)
;; (autoload 'View-scroll-half-page-forward "view")
;; (autoload 'View-scroll-half-page-backward "view")
;; (global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
;; (global-set-key (kbd "M-v") 'View-scroll-half-page-backward)


;; unbind annoying keybinds
(global-unset-key  (kbd "C-x C-n"))
(global-unset-key  (kbd "M-`"))

;; Remove whitespace from buffer on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
