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
  :disabled t
  :hook (after-init . tabspaces-mode)
  :bind (("C-c s s" . tabspaces-switch-or-create-workspace)
         ("C-c s k" . tabspaces-close-workspace)
         ("C-c s K" . tabspaces-kill-buffers-close-workspace)
         ("C-c s c" . tabspaces-clear-buffers)
         ("C-c s b" . tabspaces-switch-to-buffer)
         ("C-c s p" . tabspaces-open-or-create-project-and-workspace)
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

(use-package activities
  :straight t
  :bind (("C-c s c" . activities-new)
         ("C-c s s" . activities-switch)
         ("C-c s k" . activities-kill)
         ("C-c s l" . activities-list)
         ("C-c s r" . activities-rename)
         ("C-c s o" . activities-resume)
         ("C-c s K" . activities-discard))
  :init
  (desktop-save-mode)
  (activities-mode)
  (activities-tabs-mode))

(use-package bufferlo
  :straight t
  :bind (("C-x k" . bufferlo-remove))
  :config
  (defvar my-consult--source-buffer
    `(:name "All Buffers"
            :narrow   ?a
            :hidden   t
            :category buffer
            :face     consult-buffer
            :history  buffer-name-history
            :state    ,#'consult--buffer-state
            :items ,(lambda () (consult--buffer-query
                           :sort 'visibility
                           :as #'buffer-name)))
    "All buffer candidate source for `consult-buffer'.")

  (defvar my-consult--source-local-buffer
    `(:name nil
            :narrow   ?b
            :category buffer
            :face     consult-buffer
            :history  buffer-name-history
            :state    ,#'consult--buffer-state
            :default  t
            :items ,(lambda () (consult--buffer-query
                           :predicate #'bufferlo-local-buffer-p
                           :sort 'visibility
                           :as #'buffer-name)))
    "Local buffer candidate source for `consult-buffer'.")

  (setq consult-buffer-sources '(consult--source-hidden-buffer
                                 my-consult--source-buffer
                                 my-consult--source-local-buffer))

  :init
  (bufferlo-mode 1)
  (tab-line-mode nil))

(provide 'my-tabspaces)
