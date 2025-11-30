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
      (and init-file-had-error (reset-inhibit-vars)))))

;; Add lib/ directory to load-path
(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))

;; Load packages here
(package-initialize)

;; Ensure use-package exists
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Byte and native .el files automatically
(use-package compile-angel
  :ensure t
  :demand t
  :custom
  (compile-angel-verbose emacs-debug)
  :config
  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

;; Load custom defined functions
(org-babel-load-file (expand-file-name "defun.org" default-user-emacs-directory))
;; Load main config
(org-babel-load-file (expand-file-name "config.org" default-user-emacs-directory))

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(avy company compile-angel dashboard dockerfile-mode editorconfig embark-consult exec-path-from-shell flycheck gcmh
         git-gutter go-mode gptel gruvbox-theme highlight-indent-guides json-mode lsp-pyright magit marginalia mood-line
         nerd-icons-completion nerd-icons-dired orderless org-fragtog org-roam-timestamps org-roam-ui perspective
         php-mode popper powershell projectile rotate smartparens symbol-overlay treesit-auto use-package vertico vundo
         web-mode which-key yaml-mode yasnippet))
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
