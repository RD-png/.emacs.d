;;; my-corfu.el -*- lexical-binding: t; -*-

(use-package corfu
  :straight (corfu :repo "minad/corfu" :branch "main")
  :hook (lsp-completion-mode . corfu-setup-lsp)
  :bind (:map corfu-map
              ("<tab>" . corfu-insert)
              ("M-h" . corfu-show-documentation)
              ("M-l" . corfu-show-location)
              ("M-SPC" . corfu-insert-separator))
  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-count 10
        corfu-preview-current nil
        corfu-auto-prefix 3
        corfu-auto-delay 0.01
        corfu-quit-at-boundary t
        corfu-echo-documentation nil
        corfu-quit-no-match t
        corfu-min-width 20
        corfu-max-width 60)
  ;; (setq corfu-excluded-modes '(erlang-mode))
  (defun corfu-setup-lsp ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  
  (defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input))
    (corfu-mode 1)))
  
(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  :init
  (corfu-global-mode))

(provide 'my-corfu)
