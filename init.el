;;; init.el --- My Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; My Emacs configuration

;;; Code:

(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))

(if (<= emacs-major-version 26)
    (package-initialize))
(require 'package)

(org-babel-load-file (expand-file-name "defun.org" user-emacs-directory))
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(load custom-file)

(provide 'init)

;;; init.el ends here
