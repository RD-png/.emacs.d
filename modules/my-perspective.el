;;; my-perspective.el -*- lexical-binding: t; -*-

(use-package perspective
  :straight t
  :hook (after-init . (lambda () (persp-switch "Main") (persp-add-buffer "*Messages*")))
  :bind (("C-x C-p" . persp-switch)
         ("C-x p k" . persp-kill)
         ("C-x p n" . persp-next)
         ("C-x p p" . persp-prev)
         ("C-x p r" . persp-rename)
         ("C-c C-'" . persp-switch-last)
         ("C-x C-b" . persp-switch-to-buffer))
  :custom
  (persp-initial-frame-name "Alt")
  (persp-show-modestring t)
  (persp-modestring-short t)
  (persp-modestring-dividers '("<" ">" ""))
  (doom-modeline-persp-name t)
  :init
  (persp-mode))

(provide 'my-perspective)
