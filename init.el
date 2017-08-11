;; melpa
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa-stable" . "http://stable.melpa.org/packages/")
   t)
  (package-initialize))

;; add load-path
(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))

;; show line number
(add-hook 'prog-mode-hook #'linum-mode)

;; show mathching parenthesis
(show-paren-mode 1)
(setq show-paren-delay 0)

;; theme
(add-hook 'after-init-hook
          '(lambda()
             (if (display-graphic-p)
                 (load-theme 'solarized-dark)
               )
             (unless (display-graphic-p)
               (load-theme 'tango-dark)
               )
          ))

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; evil-mode
(require 'evil)
(evil-mode 1)
(setq evil-default-state 'emacs)

;; web-mode
(require 'web-mode)
(setq web-mode-enable-current-element-highlight t)
(setq-default indent-tabs-mode nil)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))
;; (setq-default web-mode-comment-formats
;;               '(("javascript" . "//")
;;                 ("php"        . "//")))
(setq web-mode-enable-auto-indentation nil)

;; neotree
(global-set-key [f8] 'neotree-toggle)

;; remap set mark key
(if (eq system-type 'darwin)
    (global-set-key (kbd "M-SPC") 'set-mark-command)
  )

;; camel case kill word
(global-subword-mode 1)

;; highlight library
(global-set-key (kbd "C-x y") 'hlt-highlight-symbol)
(global-set-key (kbd "C-x t") 'hlt-unhighlight-symbol)
(global-set-key (kbd "M-p") 'hlt-previous-highlight)
(global-set-key (kbd "M-n") 'hlt-next-highlight)

;; highlight-symbol
(require 'highlight-symbol)
(global-set-key (kbd "C-;") 'highlight-symbol)
(global-set-key (kbd "C->") 'highlight-symbol-next)
(global-set-key (kbd "C-<") 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
(add-hook 'window-setup-hook
          '(lambda()
             (setq highlight-symbol-colors
                   '("#ff0000" "#00ff00" "#0000ff"
                     "#ffff00" "#ff00ff" "#00ffff"
                     "#ff8000" "#ff0080" "#0080ff"))
))

;; highlight current line
(global-hl-line-mode 1)

;; hightlight white space
(global-whitespace-mode 1)
(setq whitespace-style '(face trailing tabs))
(set-face-attribute 'whitespace-tab nil :background "DeepSkyBlue")
(setq whitespace-line-column 79)

;; ido-mode
(ido-mode 1)

;; column-number-mode
(column-number-mode 1)

;; hook hs-minor-mode to add programming mode
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; flx-ido
(flx-ido-mode 1)

;; windmove key bindings
(global-set-key (kbd "C-S-b")  'windmove-left)
(global-set-key (kbd "C-S-f") 'windmove-right)
(global-set-key (kbd "C-S-p")    'windmove-up)
(global-set-key (kbd "C-S-n")  'windmove-down)

;; disable electric indent
(electric-indent-mode -1)

;; show directory when open file with the same name
(require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

;; org-mode
;; syntax highlight in code blocks
(setq org-src-fontify-natively t)

;; emojify
(setq emojify-emoji-styles '(unicode))
(add-hook 'after-init-hook #'global-emojify-mode)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; projectile-mode
(projectile-global-mode +1)

;; avy
(global-set-key (kbd "C-:") 'avy-goto-char)

;; auto revert
(global-auto-revert-mode 1)

;; eval and replace
(defun eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its value"
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))

;; toggle column marker at column 79
(require 'column-marker)
(defun toggle-column-marker (ARG)
  "Toggle column marker at column ARG
ARG defaults to 79"
  (interactive "P")
  (if (bound-and-true-p column-marker-on)
      (progn
        (column-marker-1 "")
        (setq column-marker-on nil))
    (progn
      (or ARG
          (setq ARG 79))
      (column-marker-1 ARG)
      (setq column-marker-on t))))
(global-set-key (kbd "C-'")  'toggle-column-marker)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(package-selected-packages
   (quote
    (avy js2-mode php-mode projectile flycheck emojify go-mode yaml-mode flx-ido magit web-mode solarized-theme neotree highlight-symbol evil auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
