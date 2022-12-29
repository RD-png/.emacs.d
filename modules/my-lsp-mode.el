;;; my-lsp-mode.el -*- lexical-binding: t; -*-
;; Borrowed some config from doom here to
;; avoid lsp restarting so much.

(use-package lsp-mode
  :straight t
  :preface
  (defun my/lsp-hook ()
    (lsp-deferred))

  (defun my/lsp-format-buffer ()
    (interactive)
    (lsp-format-buffer)
    (delete-trailing-whitespace))

  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :bind (:map lsp-mode-map
              ("C-c o d" . lsp-describe-thing-at-point)
              ("C-c o f" . my/lsp-format-buffer)
              ("C-c o a" . lsp-execute-code-action)
              ("C-c o R" . lsp-rename)
              ("C-c o r" . lsp-find-references)
              ("C-c o g" . lsp-find-definition))
  :custom
  (lsp-completion-provider :none)
  (lsp-auto-guess-root t)
  (lsp-enable-symbol-highlighting nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-enable-folding nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-signature-render-documentation t)
  (lsp-completion-show-detail nil)
  (lsp-eldoc-render-all nil)
  (lsp-enable-snippet t)
  (lsp-eldoc-enable-hover t)
  (lsp-document-sync-method nil)
  (lsp-signature-auto-activate nil)
  (lsp-print-performance t)
  (lsp-before-save-edits nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-signature-render-documentation t)
  (lsp-diagnostics-provider :flymake)
  :init
  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
  (setq lsp-session-file (concat user-emacs-directory "var/lsp-session")
        lsp-server-install-dir (concat user-emacs-directory "var/lsp"))
  (setq lsp-keep-workspace-alive nil)

  (add-hook 'lsp-mode-hook
            (defun +lsp-display-guessed-project-root-h ()
              "Log what LSP things is the root of the current project."
              (when-let (path (buffer-file-name (buffer-base-buffer)))
                (if-let (root (lsp--calculate-root (lsp-session) path))
                    (lsp--info "Guessed project root is %s" (abbreviate-file-name root))
                  (lsp--info "Could not guess project root."))))
            #'+lsp-optimization-mode)
  :config
  (defvar +lsp--deferred-shutdown-timer nil)
  (defadvice! +lsp-defer-server-shutdown-a (fn &optional restart)
  "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers."
  :around #'lsp--shutdown-workspace
  (if (or lsp-keep-workspace-alive
          restart
          (null +lsp-defer-shutdown)
          (= +lsp-defer-shutdown 0))
      (prog1 (funcall fn restart)
        (+lsp-optimization-mode -1))
    (when (timerp +lsp--deferred-shutdown-timer)
      (cancel-timer +lsp--deferred-shutdown-timer))
    (setq +lsp--deferred-shutdown-timer
          (run-at-time
           (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
           nil (lambda (workspace)
                 (with-lsp-workspace workspace
                                     (unless (lsp--workspace-buffers workspace)
                                       (let ((lsp-restart 'ignore))
                                         (funcall fn))
                                       (+lsp-optimization-mode -1))))
           lsp--cur-workspace))))
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

(use-package consult-lsp
  :straight t
  :bind (:map lsp-mode-map
              ("C-c o l" . consult-lsp-symbols)))

(use-package lsp-haskell
  :straight t
  :hook (haskell-mode . my/lsp-hook)
  :config
  (setq lsp-haskell-plugin-ghcide-type-lenses-global-on nil)
  (setq lsp-haskell-plugin-import-lens-code-lens-on nil)
  (setq lsp-haskell-plugin-import-lens-code-actions-on nil)
  (setq lsp-haskell-plugin-ghcide-type-lenses-config-mode nil)
  :custom
  (lsp-haskell-server-path "haskell-language-server"))

(provide 'my-lsp-mode)
