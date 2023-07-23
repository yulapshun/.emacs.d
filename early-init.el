;;; early-init.el --- My early init file -*- lexical-binding: t -*-
;;; Commentary:

;;; Commentary:
;; For optimizing startup time

;;; Code:

(setq gc-cons-threshold most-positive-fixnum)
(setq load-prefer-newer noninteractive)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

;;; early-init.el ends here
