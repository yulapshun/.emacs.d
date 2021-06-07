;; Enable and disable different modes
(show-paren-mode 1)
(evil-mode 1)
(global-subword-mode 1)
(global-hl-line-mode 1)
(global-whitespace-mode 1)
(ido-mode 1)
(column-number-mode 1)
(flx-ido-mode 1)
(electric-indent-mode 0)
(global-auto-revert-mode 1)
(eyebrowse-mode 1)
(desktop-save-mode 1)
(global-git-gutter-mode 1)
;; (add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook
          (if (>= emacs-major-version 26)
              'display-line-numbers-mode
            'linum-mode))
(add-hook 'after-init-hook 'global-emojify-mode)
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (company-flx-mode +1))

(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(add-hook 'prog-mode-hook
          '(lambda ()
             (setq display-fill-column-indicator-column 79)))

;; Set variables
(setq create-lockfiles nil)
(setq confirm-kill-emacs 'yes-or-no-p)
;; This improve performance when navigating long lines
(setq-default bidi-display-reordering nil)

;; Customize modes
(setq custom-file (concat (expand-file-name user-emacs-directory) "emacs-custom.el"))
(setq-default indent-tabs-mode nil)
(setq show-paren-delay 0)
(setq evil-default-state 'emacs)
(setq uniquify-buffer-name-style 'forward)
(setq org-src-fontify-natively t)
(setq emojify-emoji-styles '(unicode))
(setq whitespace-style '(face trailing tabs))
(setq whitespace-line-column 79)
(setq ido-auto-merge-work-directories-length nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(add-hook 'window-setup-hook
          '(lambda()
             (setq symbol-overlay-colors
                   '("#ff0000" "#00ff00" "#0000ff"
                     "#ffff00" "#ff00ff" "#00ffff"
                     "#ff8000" "#ff0080" "#0080ff"))
))
(setq symbol-overlay-map nil)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(setq neo-smart-open t)
;; (eval-after-load "company"
;;  '(add-to-list 'company-backends 'company-anaconda))

(provide 'init-misc)
