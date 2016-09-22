;; melpa
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa-stable" . "http://stable.melpa.org/packages/")
   t)
  (package-initialize))

;; show line number
(global-linum-mode 1)

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
(add-to-list 'load-path "~/.emacs.d/elpa/auto-complete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete/dict")
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
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))

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
(global-set-key (kbd "C-x '") 'highlight-symbol)
(global-set-key (kbd "C-x .") 'highlight-symbol-next)
(global-set-key (kbd "C-x ,") 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; highlight current line
(global-hl-line-mode 1)
(set-face-background hl-line-face "#222222")

;; hightlight white space
(setq whitespace-style (quote (face trailing)))
(global-whitespace-mode 1)

;; ido-mode
(ido-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
