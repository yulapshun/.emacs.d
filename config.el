(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities
      '(("gnu" . 0)
        ("melpa" . 1)))

(defvar fast-init)
(defvar completion-system 'vertico) ;; ido, vertico, ivy, helm

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(show-paren-mode 1)
(global-subword-mode 1)
(global-hl-line-mode 1)
(column-number-mode 1)
(electric-indent-mode 1)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook
          (if (>= emacs-major-version 26)
              'display-line-numbers-mode
            'linum-mode))

(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq-default create-lockfiles nil)
(setq-default bidi-display-reordering nil) ;; Improve performance when navigating long lines
(setq-default indent-tabs-mode nil)
(setq-default show-paren-delay 0)
(setq-default uniquify-buffer-name-style 'forward)
(setq-default whitespace-style '(face trailing tabs))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(add-hook 'js-mode-hook
          (lambda ()
            (setq-default js-indent-level 2)))
(add-hook 'css-mode-hook
          (lambda ()
            (setq-default css-indent-offset 2)))

(when fast-init
  (recentf-mode -1)
  (setq-default make-backup-files nil)
  (setq-default auto-save-default nil ))

(when (not fast-init)
  (global-auto-revert-mode 1)
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (when (>= emacs-major-version 27)
    (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
    (add-hook 'prog-mode-hook
              (lambda ()
                (setq display-fill-column-indicator-column 120))))
  (setq-default confirm-kill-emacs 'yes-or-no-p)
  (setq-default native-comp-deferred-compilation t)
  (require 'cheatsheet))

(global-set-key (kbd "C-x _") 'fit-window-to-buffer)
(global-set-key (kbd "C-x x") 'epa-decrypt-region)
(global-set-key (kbd "C-S-b")  'windmove-left)
(global-set-key (kbd "C-S-f") 'windmove-right)
(global-set-key (kbd "C-S-p")    'windmove-up)
(global-set-key (kbd "C-S-n")  'windmove-down)
(global-set-key (kbd "C-M-.") 'xref-find-references)

;; defun-indent
(global-set-key (kbd "<C-tab>")  'force-indent)
(global-set-key (kbd "<C-S-tab>")  'force-unindent)
(global-set-key (kbd "<C-iso-lefttab>")  'force-unindent)

;; defun-delete
(global-set-key (kbd "C-S-k")  'delete-line)
(global-set-key (kbd "C-S-w")  'delete-region)
(global-set-key (kbd "M-D")  'delete-word)
(global-set-key (kbd "<M-S-backspace>")  'backward-delete-word)

;; defun-modifier
(global-set-key (kbd "C-c g c") 'hihi-control)
(global-set-key (kbd "C-c g m") 'hihi-meta)
(global-set-key (kbd "C-c g d") 'hihi-control-shift)
(global-set-key (kbd "C-c g S") 'hihi-control-shift)
(global-set-key (kbd "C-c g j") 'hihi-meta-shift)
(global-set-key (kbd "C-c g M") 'hihi-meta-shift)
(global-set-key (kbd "C-c g x") 'hihi-control-meta)
(global-set-key (kbd "C-'") 'hihi-control-meta)

;; cheatsheet
;; (global-set-key (kbd "C-c c") 'on99-cheatsheet-open-global)
;; (global-set-key (kbd "C-c m") 'on99-cheatsheet-open-major-mode)

(defvar backup-directory (concat user-emacs-directory "backup/"))

(setq backup-directory-alist
      `((".*" . ,backup-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,backup-directory t)))

(when (not fast-init)
  (let ((month (* 60 60 24 30))
        (current (float-time (current-time))))
    (dolist (file (directory-files backup-directory t))
      (when (and (backup-file-name-p file)
                 (> (- current (float-time (cl-fifth (file-attributes file))))
                    month))
        (message "%s" file)
        (delete-file file)))))

(defun init-theme ()
  "Initialize theme."
  (load-theme 'gruvbox-dark-hard)
  (custom-theme-set-faces
   'gruvbox-dark-hard
   '(whitespace-trailing
     ;; gruvbox-dark3 and gruvbox-light3
     ((((class color) (min-colors 16777215)) (:background "#665c54" :foreground "#bdae93"))
      (((class color) (min-colors 255)) (:background "#626262" :foregroune "#a8a8a8"))))
   '(whitespace-tab
     ;; gruvbox-dark1 and gruvbox-light1
     ((((class color) (min-colors 16777215)) (:background "#3c3836" :foreground "#ebdbb2"))
      (((class color) (min-colors 255)) (:background "#3a3a3a" :foregroune "#ffdfaf"))))
   '(vertico-current
     ;; gruvbox-dark1 and gruvbox-light1
     ((((class color) (min-colors 16777215)) (:background "#504945" :foreground "#d5c4a1"))
      (((class color) (min-colors 255)) (:background "#4e4e4e" :foregroune "#bcbcbc")))))
  (enable-theme 'gruvbox-dark-hard))
(add-hook 'after-init-hook 'init-theme)
(add-hook 'after-make-frame-functions 'init-theme) ;; For emacsclient

(add-hook 'window-setup-hook
          (lambda()
            (setq-default symbol-overlay-colors
                          '("#ff0000" "#00ff00" "#0000ff"
                            "#ffff00" "#ff00ff" "#00ffff"
                            "#ff8000" "#ff0080" "#0080ff"))))

(use-package org
  :ensure t
  :defer t
  :init
  (setq-default org-src-fontify-natively t)
  (setq-default org-startup-indented t)
  (setq-default org-pretty-entities t)
  (setq-default org-hide-emphasis-markers t)
  (setq-default org-startup-with-inline-images t)
  (setq-default org-image-actual-width '(300))
  (setq-default org-use-sub-superscripts "{}")
  (setq-default org-todo-keywords
        '((sequence "TODO" "WORKING" "|" "DONE" "CLOSE")))
  (setq-default org-enforce-todo-dependencies t)
  (setq-default org-enforce-todo-checkbox-dependencies t)
  (setq-default org-priority-highest ?A)
  (setq-default org-priority-lowest ?I)
  (setq-default org-priority-default ?E)
  (setq-default org-log-done 'time)
  (setq-default org-log-done 'note)
  (let ((headline `(:inherit default :weight bold)))
    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline))))
     `(org-level-7 ((t (,@headline))))
     `(org-level-6 ((t (,@headline))))
     `(org-level-5 ((t (,@headline))))
     `(org-level-4 ((t (,@headline))))
     `(org-level-3 ((t (,@headline :height 1.1))))
     `(org-level-2 ((t (,@headline :height 1.2))))
     `(org-level-1 ((t (,@headline :height 1.3))))
     `(org-document-title ((t (,@headline :height 1.5 :underline nil))))))
  :bind
  (:map org-mode-map ("C-c C-?" . org-time-stamp-inactive))
  :custom
  (org-agenda-files "~/Sync/org/agenda"))

(use-package org-superstar
  :ensure t
  :defer t
  :custom
  (org-superstar-remove-leading-stars t)
  :hook ((org-mode . org-superstar-mode))
  :after (org))

(use-package org-roam
  :unless fast-init
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Sync/org/roam"))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  (setq-default
   org-roam-capture-templates
   '(("e" "encrypted" plain "%?"
      :target (file+head "private/%<%Y%m%d%H%M%S>-${slug}.org.gpg" "#+title: ${title} ") :unnarrowed t)
     ("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}") :unnarrowed t)))
  (setq-default
   org-roam-dailies-capture-templates
   '(("e" "encrypted" entry
      "* %?"
      :target (file+head "private/%<%Y-%m-%d>.org.gpg" "#+title: %<%Y-%m-%d>\n"))
     ("d" "default" entry
      "* %?"
      :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  (setq-default org-roam-node-display-template (concat "${title:*} " (propertize "${tags:80}" 'face 'org-tag))))

(use-package org-roam-timestamps
  :unless fast-init
  :ensure t
  :config (org-roam-timestamps-mode)
  :after (org-roam))

(use-package pyvenv
  :unless fast-init
  :ensure t
  :defer t)

(use-package pyvenv-auto
  :unless fast-init
  :ensure t
  :defer t
  :config
  (pyvenv-auto-mode t))

;; Use ido by default if emacs version is < 28 and completion-system is set to
;; vertico which does not support version < 28
(defvar use-ido
  (and (not fast-init)
       (or (eq completion-system 'ido)
           (and (< emacs-major-version 28) (eq completion-system 'vertico)))))

(when use-ido
  (ido-mode 1)
  (setq-default ido-auto-merge-work-directories-length nil)
  (setq-default ido-everywhere t))

(use-package flx-ido
  :unless (not use-ido)
  :ensure t
  :config
  (flx-ido-mode 1))

(use-package ido-completing-read+
  :unless (not use-ido)
  :ensure t
  :config
  (ido-ubiquitous-mode 1))

(use-package ido-vertical-mode
  :unless (not use-ido)
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq-default ido-vertical-show-count t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(defvar use-vertico
  (and (not fast-init)
       (>= emacs-major-version 28)
       (eq completion-system 'vertico)))

(when use-vertico
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t)
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)
  (when (>= emacs-major-version 28)
    (setq read-extended-command-predicate
          #'command-completion-default-include-p)))

(use-package vertico
  :unless (not use-vertico)
  :ensure t
  :defer t
  :init
  (vertico-mode)
  (setq vertico-cycle t)
  (setq vertico-count 12)
  (setq vertico-scroll-margin 4))

(use-package
  vertico-directory
  :unless (not use-vertico)
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package savehist
  :unless (not use-vertico)
  :init
  (savehist-mode))

(use-package marginalia
  :unless (not use-vertico)
  :bind
  (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package orderless
  :unless (not use-vertico)
  :ensure t
  :custom
  (completion-styles '(basic flex orderless))
  (completion-category-overrides
   '((file (styles basic flex partial-completion))
     (buffer (styles basic flex))
     (command (styles basic orderless))
     (symbol (styles basic))
     (variable (styles basic)))))

(use-package consult
  :unless (not use-vertico)
  :ensure t
  :defer t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c r" . consult-recent-file)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings (goto-map)
         ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g h" . consult-org-heading)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))
  (setq consult-narrow-key "<")
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package embark
  :unless (not use-vertico)
  :ensure t
  :defer t
  :bind
  (("C-c C-/" . embark-act)         ;; pick some comfortable binding
   ("C-c M-/" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings))   ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :unless (not use-vertico)
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  :after (consult embark))

(use-package all-the-icons
  :unless fast-init
  :ensure t
  :defer t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :unless fast-init
  :ensure t
  :defer t
  :after (all-the-icons)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package auto-compile
  :unless fast-init
  :ensure t
  :defer 5
  :config
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1)
  (setq-default load-prefer-newer t))

(use-package avy
  :unless fast-init
  :ensure t
  :defer 5
  :bind
  ("C-:" . 'avy-goto-char))

(use-package company
  :ensure t
  :defer 5
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq-default company-dabbrev-downcase nil)
  :bind
  (("C-." . 'company-complete)
   (:map company-active-map
         ("<tab>" . 'company-complete-common-or-cycle)
         ("C-p" . nil)
         ("C-n" . nil)
         ("M-p" . 'company-select-previous)
         ("M-n" . 'company-select-next)
         ("C-h" . 'company-show-doc-buffer))
   (:map company-search-map
         ("<tab>" . 'company-complete-common-or-cycle)
         ("C-p" . nil)
         ("C-n" . nil)
         ("M-p" . 'company-select-previous)
         ("M-n" . 'company-select-next)
         ("C-h" . 'company-show-doc-buffer)))
  :custom
  (company-idle-delay 1))

(use-package company-box
  :unless fast-init
  :hook (company-mode . company-box-mode)
  :after (company))

(use-package company-flx
  :disabled
  :ensure t
  :config
  (company-flx-mode 1)
  :after (company))

(use-package company-web
  :unless fast-init
  :ensure t
  :after (company))

(use-package compat
  :unless fast-init
  :defer t)

(use-package dashboard
  :unless fast-init
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5))))

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package elisp-benchmarks
  :unless fast-init
  :ensure t
  :defer t)

(use-package emacsql
  :unless fast-init
  :ensure t
  :defer t)

(use-package emacsql-sqlite
  :unless fast-init
  :ensure t
  :defer t)

(use-package emojify
  :disabled
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook 'global-emojify-mode)
  :config
  (setq-default emojify-emoji-styles '(unicode)))

(use-package exec-path-from-shell
  :ensure t)

(use-package eyebrowse
  :unless fast-init
  :ensure t
  :config
  (eyebrowse-mode 1))

(use-package flycheck
  :unless fast-init
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit))

(use-package git-commit
  :unless fast-init
  :defer t)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode 1))

(use-package go-mode
  :ensure t
  :defer t)

(use-package gruvbox-theme
  :ensure t
  :defer t)

(use-package js2-mode
  :disabled
  :ensure t
  :defer t
  :config
  (setq js2-basic-offset 2))

(use-package json-mode
  :ensure t
  :defer t)

(use-package lsp-mode
  :unless fast-init
  :ensure t
  :defer t
  :config
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c l M-.") #'lsp-find-definition)
    (define-key map (kbd "C-c l C-M-.") #'lsp-find-references)
    (define-key map (kbd "C-c l d") #'lsp-ui-doc-glance)
    (define-key map (kbd "C-c l i") #'lsp-ui-peek-find-implementation)
    (define-key map (kbd "C-c l I") #'lsp-find-implementation)
    (push
     `(lsp-mode . ,map)
     minor-mode-map-alist))
  :custom
  (lsp-auto-guess-root nil)
  :hook
  ((js-mode . lsp) (js2-mode . lsp) (rjsx-mode . lsp) (python-mode . lsp) (web-mode . lsp) (css-mode . lsp)
   (java-mode . lsp) (sh-mode . lsp) (html-mode . lsp) (json-mode . lsp)))
;; pip install python-lsp-server
;; npm i -g typescript-language-server
;; npm i -g vscode-json-languageserver
;; npm install -g vscode-langservers-extracted
;; npm i -g bash-language-server

(use-package lsp-ui
  :unless fast-init
  :ensure t
  :defer t
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-imenu-enable t)
  (lsp-ui-peek-always-show t)
  (lsp-ui-doc-enable nil))

(use-package magit
  :unless fast-init
  :ensure t
  :defer t
  :config
  (magit-define-popup-switch 'magit-commit-popup ?E
    "Allow empty message" "--allow-empty-message")
  (setq-default magit-completing-read-function 'magit-ido-completing-read)
  :bind
  ("C-x g" . 'magit-status))

(use-package magit-section
  :unless fast-init
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package neotree
  :unless fast-init
  :ensure t
  :defer t
  :config
  (setq-default neo-smart-open t)
  :bind
  ([f8] . 'neotree-toggle))

(use-package paredit
  :disabled
  :ensure t
  :defer t
  :hook
  ((emacs-lisp-mode . enable-paredit-mode)
   (eval-expression-minibuffer-setup . enable-paredit-mode)
   (ielm-mode . enable-paredit-mode)
   (lisp-mode . enable-paredit-mode)
   (lisp-interaction-mode . enable-paredit-mode)
   (scheme-mode . enable-paredit-mode)))

(use-package php-mode
  :ensure t
  :defer t)

(use-package popper
  :unless fast-init
  :ensure t
  :defer t
  :bind
  (("C-`" . popper-toggle-latest)
   ("C-c o o" . popper-cycle)
   ("C-c o c" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*scratch\\*"
          "error\\*$"
          "errors\\*$"
          "^\\*eshell.*" eshell-mode
          "^\\*shell.*" shell-mode
          "^\\*term.*" term-mode
          "^\\*vterm.*" vterm-mode
          "^\\*Python.*" inferior-python-mode
          help-mode
          compilation-mode))
  (setq popper-window-height 20)
  (setq popper-group-function #'popper-group-by-projectile)
  (popper-mode +1)
  (popper-echo-mode +1)
  :after (projectile))

(use-package projectile
  :unless fast-init
  :ensure t
  :defer t
  :config
  (projectile-mode 1)
  (setq-default projectile-indexing-method 'hybrid)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  :bind
  ("C-c p" . 'projectile-command-map))

(use-package pug-mode
  :ensure t
  :defer t
  :config
  (setq pug-tab-width 2)
  (setq indent-tabs-mode t))

(use-package rjsx-mode
  :disabled
  :ensure t
  :defer t
  :config
  (when (< emacs-major-version 27)
    (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))))

(use-package solarized-theme
  :disabled
  :ensure t
  :defer t)

(use-package spinner
  :unless fast-init
  :defer t)

(use-package symbol-overlay
  :ensure t
  :defer 5
  :config
  (setq-default symbol-overlay-map nil)
  :bind
  (("C-;" . 'symbol-overlay-put)
   ("C->" . 'symbol-overlay-jump-next)
   ("C-<" . 'symbol-overlay-jump-prev)
   ([(meta f3)] . 'symbol-overlay-query-replace)))

(use-package transient
  :unless fast-init
  :defer t)

(use-package undo-tree
  :ensure t
  :if (< emacs-major-version 28)
  :config (global-undo-tree-mode)
  :bind
  ((:map undo-tree-map
         ("C-x u" . 'undo-tree-visualize)
         ("C-x r u" . nil)
         ("C-x r U" . nil))))

(use-package vterm
  :if (and (string-equal system-type "gnu/linux") (not fast-init))
  :ensure t
  :defer t)

(use-package vundo
  :ensure t
  :if (>= emacs-major-version 28)
  :defer 5
  :bind
  (("C-x u" . 'vundo)))

(use-package web-mode
  :ensure t
  :defer t
  :config
  (setq-default web-mode-enable-current-element-highlight t)
  (setq-default web-mode-enable-auto-indentation nil)
  (setq-default web-mode-enable-engine-detection t)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-css-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-engines-alist '(("django" . "\\.jinja2\\'")))
  :mode
  ("\\.phtml\\'" "\\.tpl\\'"  "\\.[agj]sp\\'"  "\\.as[cp]x\\'"  "\\.erb\\'"  "\\.mustache\\'"  "\\.djhtml\\'"
  "\\.html?\\'"  "\\.xml\\'" "\\.jinja2\\'" ))

(use-package which-key
  :disabled
  :unless fast-init
  :ensure t
  :defer 1
  :init
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 100000)
  (setq which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-right-bottom))

(use-package with-editor
  :unless fast-init
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer 5)

(use-package yasnippet
  :unless fast-init
  :ensure t
  :defer t
  :hook ((emacs-lisp-mode . yas-minor-mode) (js-mode . yas-minor-mode) (python-mode . yas-minor-mode)
         (org-mode . yas-minor-mode))
  :bind (:map yas-minor-mode-map
              ("C-i" . nil) ;; Only <tab> should expand, C-i should not
              ("C-," . 'yas-expand)))

(use-package yasnippet-snippets
  :unless fast-init
  :ensure t
  :defer t)
