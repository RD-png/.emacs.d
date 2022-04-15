;;; early-init.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)
(setq native-comp-deferred-compilation nil)
(setq package-enable-at-startup nil)
(setq load-prefer-newer noninteractive)

(unless (or (daemonp)
            noninteractive
            init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq-default file-name-handler-alist nil)
    
    (defun reset-file-handler ()
      (setq file-name-handler-alist
            (delete-dups (append file-name-handler-alist
                                 old-file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'reset-file-handler 101))

  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay)))

  (define-advice load-file (:override (file) silence)
    (load file nil 'nomessage)))

(setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay)))

  (define-advice load-file (:override (file) silence)
    (load file nil 'nomessage))

(set-language-environment "UTF-8")
(setq default-input-method nil)
