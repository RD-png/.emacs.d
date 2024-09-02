;;; my-tabspaces.el -*- lexical-binding: t; -*-

(use-package tab-bar
  :straight (tab-bar :type built-in)
  :bind (("C-x C-'" . tab-bar-switch-to-recent-tab))
  :config
  (setq tab-bar-show nil))

(use-package tab-bar-echo-area
  :straight t
  :disabled t
  :after tab-bar
  :init
  (defvar tab-bar-format nil "Format for tab-bar-echo-area-mode")
  :config
  (tab-bar-echo-area-mode 1))

(use-package tabspaces
  :straight t
  :hook (after-init . tabspaces-mode)
  :bind (("C-c s s" . tabspaces-switch-or-create-workspace)
         ("C-c s k" . tabspaces-close-workspace)
         ("C-c s K" . tabspaces-kill-buffers-close-workspace)
         ("C-c s c" . tabspaces-clear-buffers)
         ("C-c s b" . tabspaces-switch-to-buffer)
         ("C-c s o" . tabspaces-open-or-create-project-and-workspace)
         ("C-c s r" . tabspaces-restore-session))
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Main")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))
  (tabspaces-session t)
  (tabspaces-session-auto-restore t))

(provide 'my-tabspaces)
