;;; my-tree-sitter.el -*- lexical-binding: t; -*-

(use-package tree-sitter
  :straight (tree-sitter :branch "master")
  :hook ((emacs-startup . global-tree-sitter-mode)
         (tree-sitter-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :straight (tree-sitter-langs :branch "master")
  :after tree-sitter
  :demand t
  :config
  (push '(typescript-react-mode . typescript)
        tree-sitter-major-mode-language-alist)
  (push '(web-mode . html)
        tree-sitter-major-mode-language-alist))

(provide 'my-tree-sitter)
