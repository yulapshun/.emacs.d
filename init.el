;;; init.el --- My Emacs configuration -*- no-byte-compile: t; lexical-binding: t -*-

;;; Commentary:
;; My Emacs configuration

;;; Code:

;; Optimize startup, copied from Centaur Emacs
(unless (or (daemonp) noninteractive init-file-debug)
  ;; Prevent flashing of messages at startup
  (when (display-graphic-p)
    (setq-default inhibit-redisplay t
                  inhibit-message t)
    (defun reset-inhibit-vars ()
      (setq-default inhibit-redisplay nil
                    inhibit-message nil)
      (redraw-frame))
    (add-hook 'window-setup-hook #'reset-inhibit-vars)
    (define-advice startup--load-user-init-file (:after (&rest _) reset-inhibit-vars)
      (and init-file-had-error (reset-inhibit-vars))))

  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))

(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package compile-angel
  :ensure t
  :demand t
  :custom
  (compile-angel-verbose nil)
  :config
  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

(org-babel-load-file (expand-file-name "defun.org" init-user-emacs-directory))
(org-babel-load-file (expand-file-name "config.org" init-user-emacs-directory))

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rotate lsp-pyright which-key lsp-mode nerd-icons nerd-icons-completion nerd-icons-dired company eyebrowse vundo good-scroll yasnippet-snippets yaml-mode web-mode vertico use-package treesit-auto symbol-overlay projectile php-mode org-roam-ui org-roam-timestamps org-fragtog orderless mood-line markdown-mode marginalia magit json-mode gruvbox-theme go-mode git-gutter gcmh flycheck exec-path-from-shell embark-consult dockerfile-mode dashboard compile-angel avy auto-virtualenv))
 '(safe-local-variable-values '((org-pretty-entities))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:inherit default :weight bold :height 1.5 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :height 1.3))))
 '(org-level-2 ((t (:inherit default :weight bold :height 1.2))))
 '(org-level-3 ((t (:inherit default :weight bold :height 1.1))))
 '(org-level-4 ((t (:inherit default :weight bold))))
 '(org-level-5 ((t (:inherit default :weight bold))))
 '(org-level-6 ((t (:inherit default :weight bold))))
 '(org-level-7 ((t (:inherit default :weight bold))))
 '(org-level-8 ((t (:inherit default :weight bold)))))
