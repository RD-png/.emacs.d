;;; my-company.el -*- lexical-binding: t; -*-

(use-package company
  :straight t
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :init
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.01
        company-tooltip-limit 10
        company-tooltip-align-annotations t
        company-require-match 'never
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-auto-commit nil
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)
  (global-company-mode +1))

(provide 'my-company)
