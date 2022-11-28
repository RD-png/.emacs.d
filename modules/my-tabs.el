;;; tabs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun tab-create (name)
  (condition-case nil
      (unless (equal (alist-get 'name (tab-bar--current-tab))
                     name)
        (tab-bar-rename-tab-by-name name name))
    (error (tab-new)
           (tab-bar-rename-tab name))))

(use-package tab-bar
  :straight (tab-bar :type built-in)
  ;; :hook (after-init . (lambda ()
  ;;                       (doom-modeline-def-segment workspace-name
  ;;                         (when doom-modeline-workspace-name
  ;;                           (when-let
  ;;                               ((name (cond
  ;;                                       (t
  ;;                                        (let* ((current-tab (tab-bar--current-tab))
  ;;                                               (tab-index (tab-bar--current-tab-index))
  ;;                                               (explicit-name (alist-get 'name current-tab))
  ;;                                               (tab-name (alist-get 'name current-tab)))
  ;;                                          (if explicit-name tab-name (+ 1 tab-index)))))))
  ;;                             (propertize (format " %s " name) 'face
  ;;                                         (if (doom-modeline--active)
  ;;                                             'doom-modeline-buffer-major-mode
  ;;                                           'mode-line-inactive)))))))
  :config
  (tab-bar-history-mode 1)
  (setq  tab-bar-close-last-tab-choice 'tab-bar-mode-disable
         tab-bar-show                   nil
         tab-bar-new-tab-choice        'ibuffer
         tab-bar-tab-name-truncated-max 14
         ;; This makes tramp really laggy.

         ;; tab-bar-tab-name-function #'(lambda nil (let ((path (my/project-current-root)))
         ;;                                           (if path
         ;;                                               (f-base path)
         ;;                                             (f-base default-directory))))
         )
  (defun my/vertico-tab-source ()
    (setq buffer-names-to-keep
          (append (mapcar #'buffer-name (alist-get 'wc-bl (tab-bar--tab)))
                  (mapcar #'buffer-name (alist-get 'wc-bbl (tab-bar--tab)))))
    `(:name ,(s--aget (cdr (tab-bar--current-tab)) 'name)
            :hidden nil
            :narrow ?0
            :category buffer
            :state ,#'consult--buffer-state
            :items ,(lambda ()
                      (consult--buffer-query
                       :sort 'visibility
                       :as #'buffer-name
                       :predicate (lambda (buf)
                                    (when (member (buffer-name buf) buffer-names-to-keep)
                                      t))))))
  (defun my/switch-tab-bar-buffer ()
    (interactive)
    (when-let (buffer (consult--multi (list (my/vertico-tab-source))
                                      :require-match
                                      (confirm-nonexistent-file-or-buffer)
                                      :prompt (format "Switch to buffer (%s): "
                                                      (s--aget (cdr (tab-bar--current-tab)) 'name))
                                      :history 'consult--buffer-history
                                      :sort nil))
      (unless (cdr buffer)
        (funcall consult--buffer-display (car buffer)))))

  :bind-keymap ("C-c t" . tab-prefix-map)
  :bind
  ("C-x C-'" . tab-bar-switch-to-recent-tab)
  (:map tab-prefix-map
        ("n" . tab-bar-switch-to-next-tab)
        ("p" . tab-bar-switch-to-prev-tab)
        ("N" . tab-bar-history-forward)
        ("P" . tab-bar-history-back))
  :bind*
  ("C-x C-b" . my/switch-tab-bar-buffer)
  ("C-x C-p" . tab-switch)
  :init
  (tab-create "Alt")
  (tab-create "Main")
  (tab-bar-close-tab 1)
  (tab-bar-mode -1))

(use-package tab-bar-echo-area
  :straight t
  :after tab-bar
  :init
  (defvar tab-bar-format nil "Format for tab-bar-echo-area-mode")
  :config
  (tab-bar-echo-area-mode 1))

(provide 'my-tabs)
