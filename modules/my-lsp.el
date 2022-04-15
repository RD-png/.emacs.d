;;; my-lsp.el -*- lexical-binding: t; -*-

(use-package lsp-mode
  :straight t
  :preface
  (defun my/lsp-format-buffer ()
    (interactive)
    (lsp-format-buffer)
    (delete-trailing-whitespace))
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
  (lsp-enable-on-type-formatting nil)
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
  (lsp-diagnostics-provider :flymake)
  :init
  (defun my/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

(use-package lsp-volar
  :straight (lsp-volar :host github :repo "jadestrong/lsp-volar"))

(use-package lsp-haskell
  :straight t
  :hook (haskell-mode . lsp-deferred)
  :custom
  (lsp-haskell-server-path "haskell-language-server"))

(provide 'my-lsp)
