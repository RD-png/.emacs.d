;;; my-langs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun mode-unicode-conversions (modes unicode-chars)
  (mapc (lambda (mode)
            (font-lock-add-keywords
             mode
             (append (mapcar (lambda (chars)
                               `(,(car chars)
                                 ,(if (characterp (cdr chars))
                                      `(0 (ignore
                                           (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           ,(cdr chars))))
                                    `(0 ,(cdr chars)))))
                             unicode-chars))))
        modes))

(use-package prog-mode
  :straight (prog-mode :type built-in)
  :hook (prog-mode . display-line-numbers-mode)
  :hook (prog-mode . display-fill-column-indicator-mode)
  :hook (before-save . delete-trailing-whitespace))

(use-package conf-mode
  :straight (conf-mode :type built-in)
  :hook (conf-mode . display-line-numbers-mode))

(use-package php-mode
  :straight t
  :mode "\\.php\\'"
  :hook (php-mode . my/lsp-hook))

(use-package markdown-mode
  :straight t
  :hook (markdown-mode . display-line-numbers-mode))

(use-package ts
  :straight t)

(use-package htmlize
  :straight t
  :config
  (setq org-html-htmlize-output-type 'css))

(use-package pip-requirements
  :straight t
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

(use-package python-mode
  :straight t
  :hook (python-mode . my/lsp-hook)
  :bind (:map python-mode-map
              ([remap lsp-format-buffer] . python-black-buffer))
  :config
  (setq python-shell-interpreter "python3")
  (setq python-indent-offset 4)
  (add-hook 'python-mode-hook
            (lambda()
              (local-unset-key (kbd "DEL")))))

(use-package pyimport
  :straight t
  :after python-mode)

(use-package pyvenv
  :straight
  :defer 5
  :config
  (setq pyvenv-menu t))

(use-package python-black
  :straight t
  :defer 5)

(use-package nix-mode
  :straight t
  :mode "\\.nix\\'"
  :hook (nix-mode . my/lsp-hook))

(use-package nix-update
  :straight t
  :commands nix-update-fetch)

(use-package web-mode
  :straight t
  :mode ("\\.vue\\'")
  :hook (web-mode . my/lsp-hook)
  :config
  (setq web-mode-enable-html-entities-fontification t
        web-mode-auto-close-style 1
        web-mode-indent-style 2
        web-mode-attr-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-style-padding 0
        web-mode-script-padding 0
        web-mode-enable-auto-quoting nil
        web-mode-enable-auto-pairing t)
  (setf (alist-get "javascript" web-mode-comment-formats nil nil #'equal) "//"))

(use-package css-mode
  :straight t
  :mode ("\\.css\\'"))

(use-package haskell-mode
  :straight t
  :mode ("\\.hs\\'")
  :hook (haskell-mode . my/lsp-hook)
  :hook (haskell-mode . dumb-jump-mode)
  :bind (:map haskell-mode-map
              ("C-c C-c" . haskell-compile)
              ([remap my/lsp-format-buffer] . haskell-mode-stylish-buffer))
  :config
   (setq haskell-process-suggest-remove-import-lines t
         haskell-process-auto-import-loaded-modules t
         haskell-process-type 'cabal-repl)
  :preface
  (defconst haskell-unicode-conversions
  '(("[ (]\\(\\.\\)[) ]"       . ?∘)
    ("[ (]\\(\\\\\\)[(_a-z]"   . ?λ)
    ("[ (]\\(\\<not\\>\\)[ )]" . ?¬)))
  :init
  (mode-unicode-conversions '(haskell-mode haskell-literate-mode) haskell-unicode-conversions))

(use-package typescript-mode
  :straight t
  :mode
  ("\\.ts\\'"
   "\\.js\\'")
  :hook (typescript-mode . my/lsp-hook)
  :config
  (setq typescript-indent-level 4))

(use-package emacs-lisp-mode
  :straight (emacs-lisp-mode :type built-in)
  :hook (lisp-mode . emacs-lisp-mode)
  :hook (company-mode . (lambda () (set-company-backend! 'emacs-lisp-mode

                                '(company-elisp :with company-yasnippet company-files))))
  :bind ((:map emacs-lisp-mode-map
               ("C-c m s" . ielm)))
  :preface
  (defconst elisp-unicode-conversions
  '(("[ (]\\(\\lambda\\)[) ]"       . ?λ)))
  :init
  (mode-unicode-conversions '(emacs-lisp-mode) elisp-unicode-conversions))

(use-package scheme-mode
  :straight (scheme-mode :type built-in)
  :mode ("\\.sld\\'"))

(use-package racket-mode
  :straight t
  :mode ("\\.rkt\\'"))

(use-package go-mode
  :straight t
  :mode ("\\.go\\'")
  :hook(go-mode . my/lsp-hook)
  :config
  (setq lsp-go-analyses
        '((fieldalignment . t)
          (nilness . t)
          (unusedwrite . t)
          (unusedparams . t)))
  (with-eval-after-load 'lsp-mode
    (lsp-register-custom-settings
     '(("gopls.completeUnimported" t t)
       ("gopls.staticcheck" t t)
       ("gopls.experimentalWorkspaceModule" t t))))
  (setq lsp-gopls-server-path "/home/ryan/go/bin/gopls"))

(use-package rustic
  :straight t
  :mode ("\\.rs$" . rustic-mode)
  :hook (rustic-mode-hook . rustic-lsp-mode-setup)
  :config
  (setq rustic-lsp-server 'rls)
  (setq rustic-lsp-server 'rustfmt)
  (setq rustic-lsp-client 'lsp-mode)
  (setq rustic-indent-method-chain t))

(use-package erlang
  :straight t
  :mode ("\\.erlang\\'" . erlang-mode)
  :mode ("/rebar\\.config\\(?:\\.script\\)?\\'" . erlang-mode)
  :mode ("/\\(?:app\\|sys\\)\\.config\\'" . erlang-mode)
  :hook (erlang-mode . my/lsp-hook)
  :hook (erlang-mode . erlang-edoc-mode)
  :bind ((:map erlang-mode-map
              ("C-c m s" . erlang-shell-rebar)
              ("C-c C-c" . recompile)
              ([remap consult-eglot-symbols] . consult-imenu)
              ([remap erlang-electric-newline] . newline-and-indent))
         :map erlang-shell-mode-map
         ("C-c C-g" . comint-interrupt-subjob)
         ("C-c C-c" . erlang-shell-rebar-reload)
         ("C-c m s" . erlang-shell-rebar))
  :config
  (defun erlang-shell-rebar ()
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (inferior-erlang "make dev")))
  (defun erlang-shell-rebar-reload ()
    (interactive)
    (comint-interrupt-subjob)
    (kill-buffer)
    (erlang-shell-rebar))
  (setq erlang-indent-level 2)
  (setq erlang-indent-guard 2)
  (setq erlang-icr-indent 2)
  (setq inferior-erlang-machine-options '("-sname" "emacs")))

(use-package tuareg
  :straight t
  :mode ("\\.ml$" . tuareg-mode)
  :bind (("C-c m s" . utop))
  :hook (tuareg-mode . my/lsp-hook))

(use-package utop
  :straight t)

(use-package yaml-mode
  :straight t
  :mode ("\\.yaml$" . yaml-mode))

(use-package latex
  :straight (latex :type built-in)
  :defer 5
  :after tex
  :mode ("\\.tex\\'" . LaTeX-mode))

;; (use-package auctex
;;   :straight (auctex :type built-in))

(use-package cdlatex
  :straight (cdlatex :type built-in)
  :defer 5
  :after latex
  :hook (LaTeX-mode . turn-on-cdlatex))

(use-package eldoc
  :straight (eldoc :type built-in)
  :custom
  (eldoc-idle-delay 0)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-display-truncation-message nil))

(use-package devdocs
  :straight t
  :defer 2
  :config
  (defun my/devdocs-lookup ()
    (interactive)
    (devdocs-lookup nil (thing-at-point 'word 'no-properties)))
  (add-hook 'web-mode-hook
            (lambda () (setq-local devdocs-current-docs '("vue~3"))))
  (add-hook 'python-mode-hook
            (lambda () (setq-local devdocs-current-docs '("django_rest_framework" "django~3.2"))))
  (add-hook 'php-mode-hook
            (lambda () (setq-local devdocs-current-docs '("laravel~8"))))
  :bind ("C-c o D" . my/devdocs-lookup))

(provide 'my-langs)
