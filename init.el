;; Add load-path
(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "defun" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

(require 'package)
(add-to-list
 'package-archives
 '("melpa-stable" . "http://stable.melpa.org/packages/")
 t)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
(setq package-archive-priorities
      '(("gnu"          . 1)
        ("melpa-stable" . 0)
        ("melpa"        . -1000)))
(package-initialize)

(require 'defun-misc)
(require 'defun-indent)
(require 'defun-delete)

(require 'init-misc)
(require 'init-major-mode)
(require 'init-keys)
(require 'init-ui)
(require 'init-web-mode)
(require 'init-paredit)
(require 'init-magit)

(load custom-file)
