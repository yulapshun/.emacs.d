;;; init.el --- My Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; My Emacs configuration
;; Supports Emacs version >= 26

;;; Code:

(defvar fast-init nil)

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
