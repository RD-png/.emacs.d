;;; my-workflow.el -*- lexical-binding: t; -*-

(use-package project
  :straight (project :type built-in)
  :demand t
  :init
  (global-set-key (kbd "C-c p") project-prefix-map)
  (cl-defgeneric project-root (project) (car project))
  (defun my/project-current-root (&optional dir)
    (when-let ((project
                (project-current nil (or dir default-directory))))
      (project-root project)))
  :bind* (("C-c p f" . project-find-file)
          ("C-c p s r" . project-find-regexp)
          ("C-c p d" . project-dired)
          ("C-c p b" . project-switch-to-buffer)
          ("C-c p r" . project-query-replace-regexp)
          ("C-c p v" . project-vc-dir)
          ("C-c p k" . project-kill-buffers)
          ("C-c p !" . project-shell-command)
          ("C-c p e" . project-eshell)
          ("C-c p g" . consult-ripgrep)))


(use-package magit
  :straight t
  :commands (magit-status magit-get-current-branch)
  :bind ("C-c g" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :straight t
  :after magit)

(use-package git-gutter
  :straight t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:disabled-modes '(fundamental-mode image-mode pdf-view-mode))
  (setq git-gutter:handled-backends
        (cons 'git (cl-remove-if-not #'executable-find (list 'hg 'svn 'bzr)
                                     :key #'symbol-name)))
  (setq git-gutter:update-interval 0.02)
  (defun git-gutter:start-of-hunk ()
    "Move to end of current diff hunk"
    (interactive)
    (git-gutter:awhen (git-gutter:search-here-diffinfo git-gutter:diffinfos)
      (let ((lines (- (git-gutter-hunk-start-line it) (line-number-at-pos))))
        (forward-line lines))))
  :bind
  ("C-c C-v t" . git-gutter-mode)
  ("C-c C-v r" . git-gutter:revert-hunk)
  ("C-c C-v m" . git-gutter:mark-hunk)
  ("C-c C-v n" . git-gutter:next-hunk)
  ("C-c C-v p" . git-gutter:previous-hunk)
  ("C-c C-v s" . git-gutter:stage-hunk)
  ("C-c C-v g" . git-gutter:update-all-windows)
  ("C-c C-v d" . git-gutter:popup-hunk)
  ("C-c C-v e" . git-gutter:end-of-hunk)
  ("C-c C-v a" . git-gutter:start-of-hunk))

(use-package git-gutter-fringe
  :straight t
  :config
  (setq-default fringes-outside-margins t)
  (define-fringe-bitmap 'git-gutter-fr:added [0] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [0] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package pdf-tools
  :straight t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install-noverify)
  (setq-default pdf-view-display-size 'fit-width)
  (add-hook 'pdf-annot-list-mode-hook #'hide-mode-line-mode))

(use-package saveplace-pdf-view  
  :straight t
  :after pdf-view
  :init
  (save-place-mode 1))

(provide 'my-workflow)
