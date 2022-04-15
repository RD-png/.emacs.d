;;; my-consult.el -*- lexical-binding: t; -*-

(use-package consult
  :straight t
  :demand t
  :after project
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
  :bind (("C-s" . consult-line)
         ("C-M-m" . consult-imenu)
         ("C-M-S-m" . consult-imenu-multi)
         ("C-M-s" . consult-multi-occur)
         ("C-M-l" . consult-outline)
         ("M-g M-g" . consult-goto-line)
         ("C-c f" . consult-flymake)
         ("C-x M-f" . consult-recent-file)
         ([remap yank-pop] . consult-yank-pop)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple))

(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-map
         ("C-x j" . consult-dir-jump-file)))

(use-package consult-lsp
  :straight t
  :bind (("C-c o l" . consult-lsp-symbols)))

(provide 'my-consult)
