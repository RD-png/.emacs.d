;;; my-vertico.el -*- lexical-binding: t; -*-

(use-package vertico
  :straight (vertico :repo "minad/vertico"
                     :branch "main")
  :demand t
  :preface
  (defun consult-vertico-save ()
    "Cleaner version of vertico-save that works with
consult based prompts."
    (interactive)
    (embark--act #'kill-new (car (embark--targets)))
    (abort-minibuffers))
  :config
  (setq
   vertico-count 7
   vertico-cycle t
   vertico-resize nil)
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)
  :bind (:map vertico-map
              ("M-w" . consult-vertico-save)
              ("C-M-a" . vertico-first)
              ("C-M-e" . vertico-last))
  :init
  (vertico-mode))

(provide 'my-vertico)
