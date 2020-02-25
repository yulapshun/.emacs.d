;; Add load-path
(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "defun" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)
(setq package-archive-priorities
      '(("gnu"          . 1)
        ("melpa"        . 0)))
(if (<= emacs-major-version 26)
              (package-initialize))

(require 'defun-misc)
(require 'defun-indent)
(require 'defun-delete)
(require 'defun-modifier)
;; (require 'defun-input)

(require 'init-misc)
(require 'init-major-mode)
(require 'init-keys)
(require 'init-backup)
(require 'init-ui)
(require 'init-web-mode)
(require 'init-paredit)
(require 'init-magit)
(require 'init-projectile)
(require 'init-lsp-mode)

(load custom-file)
