;;; early-init.el --- My early init file -*- lexical-binding: t -*-
;;; Commentary:

;;; Commentary:
;; For optimizing startup time

;;; Code:

(defvar file-name-handler-alist-original file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil
      load-prefer-newer noninteractive)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 100000000
                  gc-cons-percentage 0.1
                  file-name-handler-alist file-name-handler-alist-original)))

(setq package-enable-at-startup nil)
(setq load-prefer-newer noninteractive)
(setq frame-inhibit-implied-resize t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;;; early-init.el ends here
