;;; my-orderless.el -*- lexical-binding: t; -*-

(use-package orderless
  :straight t
  :preface
  (defun literal-if-equals (pattern _index _total)
    (when (string-prefix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 1))))

  (defun without-if-bang (pattern _index _total)
    "Exclude literal when leading punctuation-mark."
    (when (string-prefix-p "!" pattern)
      (if (eq pattern "!")
          `(orderless-literal . "")
        `(orderless-without-literal . ,(substring pattern 1)))))
  :custom
  (orderless-matching-styles '(orderless-regexp))
  (orderless-style-dispatchers '(literal-if-equals
                                 without-if-bang))
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(provide 'my-orderless)
