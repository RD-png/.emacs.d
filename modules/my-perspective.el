;;; my-perspective.el -*- lexical-binding: t; -*-

(use-package perspective
  :straight t
  :hook (after-init . (lambda () (persp-switch "Main") (persp-add-buffer "*Messages*")))
  :bind (("C-c t b" . persp-switch)
         ("C-c t k" . persp-kill)
         ("C-c t n" . persp-next)
         ("C-c t p" . persp-prev)
         ("C-c t r" . persp-rename)
         ("C-x C-'" . persp-switch-last)
         ("C-c t C-b" . persp-switch-to-buffer))
  :custom
  (persp-initial-frame-name "Alt")
  (persp-show-modestring t)
  (persp-modestring-short t)
  (persp-modestring-dividers '("<" ">" ""))
  (doom-modeline-persp-name t)
  (persp-suppress-no-prefix-key-warning t)
  :init
  (persp-mode))

(provide 'my-perspective)
