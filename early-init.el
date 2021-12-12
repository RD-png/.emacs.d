;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(setq package-enable-at-startup nil
      load-prefer-newer t)

(setq-default inhibit-message t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)
(setq inhibit-x-resources nil)

(set-language-environment "UTF-8")
