;;; early-init.el --- My early init file -*- no-byte-compile: t; lexical-binding: t -*-

;;; Commentary:
;; For optimizing startup time

;;; Code:

(defvar emacs-debug nil
  "Non-nil to enable debug.")
(defvar init-user-emacs-directory user-emacs-directory
  "The default value of the `user-emacs-directory' variable.")
(defvar file-name-handler-alist-original file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil
      load-prefer-newer noninteractive
      package-enable-at-startup nil
      frame-inhibit-implied-resize t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1
                  file-name-handler-alist file-name-handler-alist-original)))

;; Reducing clutter in ~/.emacs.d by redirecting files to ~/emacs.d/var/
(setq user-emacs-directory (expand-file-name "var/" init-user-emacs-directory))

(setq load-prefer-newer t)

(setq native-comp-jit-compilation t)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; Suppress compiler warnings and don't inundate users with their popups.
(setq native-comp-async-report-warnings-errors
      (or emacs-debug 'silent))
(setq native-comp-warning-on-missing-source emacs-debug)

(setq debug-on-error emacs-debug
      jka-compr-verbose emacs-debug)

(setq byte-compile-warnings emacs-debug)
(setq byte-compile-verbose emacs-debug)

;;; early-init.el ends here
