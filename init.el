;;; init.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;;; Native Comp.
(custom-set-variables
 '(warning-suppress-log-types '((org-element-cache))))
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq comp-deferred-compilation t)
    (setq warning-minimum-level :error)
    (setq package-native-compile t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))))


;;; Straight.
(setq straight-use-package-by-default t
      straight-disable-native-compile nil
      straight-check-for-modifications nil
      straight-vc-git-default-clone-depth 1
      autoload-compute-prefixes nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
			             ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(require 'use-package)
(require 'straight-x)


;;; Core Modules.
(require 'my-doom)
(require 'core)
(require 'ui)
(require 'my-tree-sitter)
(require 'my-vertico)
;; (require 'my-corfu)
(require 'my-company)
(require 'my-orderless)
(require 'my-completion)
(require 'my-consult)
(require 'my-embark)
(require 'my-navigation)
(require 'my-lsp)
(require 'my-langs)
(require 'my-perspective)
(require 'my-workflow)
;; (require 'my-tabs)
(require 'my-editing)
(require 'my-org)
;; (require 'my-eshell)
(require 'my-vterm)
(require 'my-helpers)
(require 'my-theme)
