;; Enable and disable different modes
(show-paren-mode t)
(evil-mode t)
(ac-config-default)
(global-subword-mode t)
(global-hl-line-mode t)
(global-whitespace-mode t)
(ido-mode t)
(column-number-mode t)
(flx-ido-mode t)
(electric-indent-mode nil)
(global-auto-revert-mode t)
(eyebrowse-mode t)
(projectile-global-mode t)
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'after-init-hook 'global-emojify-mode)
(add-hook 'after-init-hook 'global-flycheck-mode)

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
             (setq highlight-symbol-colors
                   '("#ff0000" "#00ff00" "#0000ff"
                     "#ffff00" "#ff00ff" "#00ffff"
                     "#ff8000" "#ff0080" "#0080ff"))
))
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(provide 'init-misc)
