;;; early-init.el --- My early init file -*- no-byte-compile: t; lexical-binding: t -*-

;;; Commentary:
;; Mainly for optimizing startup time

;;; Code:

(defvar emacs-debug nil
  "Non-nil to enable debug.")
(defvar default-gc-cons-threshold gc-cons-threshold
  "The default value of the `gc-cons-threshold' variable.")
(defvar default-gc-cons-percentage gc-cons-percentage
  "The default value of the `gc-cons-percentage' variable.")
(defvar default-user-emacs-directory user-emacs-directory
  "The default value of the `user-emacs-directory' variable.")
(defvar default-file-name-handler-alist file-name-handler-alist
  "The default value of the `file-name-handler-alist' variable.")

;; Optimization to speed up emacs startup
(setq
;; Prevent garbage collection
 gc-cons-threshold most-positive-fixnum
;; Prevent garbage collection
 gc-cons-percentage 0.6
;; Prevent handling of specially handled function
 file-name-handler-alist nil
;; Prevent loading of site-start.el
 site-run-file nil
;; Load old version of files when running non interactively
 load-prefer-newer noninteractive
;; Make packages only available after init.el
 package-enable-at-startup nil
;; Prevent the frame from being resized by UI elements
 frame-inhibit-implied-resize t)

;; Set variables back to their original values after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold default-gc-cons-threshold
                  gc-cons-percentage default-gc-cons-percentage
                  file-name-handler-alist default-file-name-handler-alist)))

;; Reducing clutter in ~/.emacs.d by redirecting files to ~/emacs.d/var/
(setq user-emacs-directory (expand-file-name "var/" default-user-emacs-directory))
(setq load-prefer-newer t)
;; Native compile .elc files asynchronously
(setq native-comp-jit-compilation t)

;; Disable UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
;; Enable transparent titlebar on Mac
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; Suppress compiler warnings and don't inundate users with their popups.
(setq native-comp-async-report-warnings-errors emacs-debug)
(setq native-comp-warning-on-missing-source emacs-debug)
(setq debug-on-error emacs-debug
      jka-compr-verbose emacs-debug)
(setq byte-compile-warnings emacs-debug)
(setq byte-compile-verbose emacs-debug)

;;; early-init.el ends here
