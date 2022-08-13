;;; init.el --- My Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; My Emacs configuration

;;; Code:

(setq gc-cons-threshold 100000000)
(setq read-process-output-max 1048576)
(tool-bar-mode -1)
(menu-bar-mode -1)

(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))

(if (or (<= emacs-major-version 26) fast-init)
    (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(if fast-init
    (progn
      (load-file (expand-file-name "defun.el" user-emacs-directory))
      (load-file (expand-file-name "config.el" user-emacs-directory)))
  (progn
    (org-babel-load-file (expand-file-name "defun.org" user-emacs-directory))
    (org-babel-load-file (expand-file-name "config.org" user-emacs-directory))))

(load custom-file)

(provide 'init)

;;; init.el ends here
