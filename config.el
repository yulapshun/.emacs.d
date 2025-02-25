;; -*- no-byte-compile: t; lexical-binding: t -*-

(use-package emacs
  :config
  ;; Add melpa to package-archives make it preferred over gnu archive
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (setq-default package-archive-priorities
                '(("melpa" . 1)))
  ;; Highlight mathing parenthesis
  (show-paren-mode 1)
  ;; Make word based navigation easier
  (global-subword-mode 1)
  ;; Highlight current line
  (global-hl-line-mode 1)
  ;; Show the current column number in the mode line
  (column-number-mode 1)
  ;; Auto indent afetr every new line
  (electric-indent-mode 1)
  ;; Revert a buffer automatically if it is changed on disk
  (global-auto-revert-mode 1)
  ;; Replace selected text with typed text
  (delete-selection-mode 1)
  ;; Save mimibuffer history
  (savehist-mode 1)
  (setq-default
   ;; Use space for indentation by default
   indent-tabs-mode nil
   show-paren-delay 0
   ;; Make buffer name look like the file path
   uniquify-buffer-name-style 'forward
   ;; Visualize trailing whitespaces and tabs
   whitespace-style '(face trailing tabs)
   ;; Confirm before exiting emacs
   confirm-kill-emacs 'y-or-n-p
   ;; Show fill column indicator at 120
   fill-column 120)
  ;; Enable upcase-region and downcase-region
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  ;; use 'y' or 'n' instead of 'yes' or 'no' everywhere
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Ensure path is set up correctly
  (exec-path-from-shell-initialize)
  :hook
  (
   ;; Visualize whitespaces
   (prog-mode . whitespace-mode)
   ;; Show line numbers
   (prog-mode . display-line-numbers-mode)
   ;; Enable line wrapping
   (prog-mode . visual-line-mode)
   ;; Enable code folding
   (prog-mode . hs-minor-mode)
   ;; Show column indicator
   (prog-mode . display-fill-column-indicator-mode)))

(global-set-key (kbd "C-C j x") #'epa-decrypt-region)

;; windmove
(global-set-key (kbd "C-S-b") #'windmove-left)
(global-set-key (kbd "C-S-f") #'windmove-right)
(global-set-key (kbd "C-S-p") #'windmove-up)
(global-set-key (kbd "C-S-n") #'windmove-down)
(global-set-key (kbd "C-c j b") #'windmove-left)
(global-set-key (kbd "C-c j f") #'windmove-right)
(global-set-key (kbd "C-c j p") #'windmove-up)
(global-set-key (kbd "C-c j n") #'windmove-down)

;; defun-indent
(global-set-key (kbd "<C-tab>") #'my/force-indent)
(global-set-key (kbd "<C-S-tab>") #'my/force-unindent)
(global-set-key (kbd "<C-iso-lefttab>") #'my/force-unindent)
(global-set-key (kbd "C-c j <tab>") #'my/force-indent)
(global-set-key (kbd "C-c j <backtab>") #'my/force-unindent)

;; defun-delete
(global-set-key (kbd "C-S-k") #'my/delete-line)
(global-set-key (kbd "C-S-w") #'delete-region)
(global-set-key (kbd "M-D") #'my/delete-word)
(global-set-key (kbd "<M-S-backspace>") #'my/backward-delete-word)
(global-set-key (kbd "C-c j k") #'my/delete-line)
(global-set-key (kbd "C-c j w") #'my/delete-region)
(global-set-key (kbd "C-c j d") #'my/delete-word)
(global-set-key (kbd "C-c j <del>") #'my/backward-delete-word)

;; defun-modifier
(global-set-key (kbd "C-c j c") #'my/control)
(global-set-key (kbd "C-c j m") #'my/meta)
(global-set-key (kbd "C-c j S") #'my/control-shift)
(global-set-key (kbd "C-c j M") #'my/meta-shift)
(global-set-key (kbd "C-c j z") #'my/control-meta)

(setq auto-save-list-file-prefix nil)
(defvar auto-save-directory (concat user-emacs-directory "autosave/"))
(defvar backup-directory (concat user-emacs-directory "backup/"))

(setq auto-save-file-name-transforms `((".*" ,auto-save-directory t)))
(setq backup-directory-alist `((".*" . ,backup-directory)))

(let ((month (* 60 60 24 30))
      (current (float-time (current-time))))
  (dolist (file (directory-files backup-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (cl-fifth (file-attributes file))))
                  month))
      (message "%s" file)
      (delete-file file))))

(use-package gruvbox-theme
  :ensure t
  :defer t)

(defun init-theme ()
  "Initialize theme."
  (load-theme 'gruvbox-dark-hard t)
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

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((recents  . 10)
                          (projects . 10))))

(use-package mood-line
  :config
  (mood-line-mode)
  :custom
  (mood-line-glyph-alist  mood-line-glyphs-fira-code)
  :custom-face
  (mood-line-buffer-status-read-only ((t (:inherit mode-line-inactive))))
  (mood-line-encoding ((t (:inherit mode-line-inactive))))
  (mood-line-status-neutral ((t (:inherit mode-line-inactive))))
  (mood-line-unimportant ((t (:inherit mood-line-unimportant)))))

(use-package highlight-indent-guides
  :hook ((prog-mode . highlight-indent-guides-mode))
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-auto-enabled nil)
  :custom-face
  (highlight-indent-guides-odd-face ((t (:foreground "darkgray"))))
  (highlight-indent-guides-even-face ((t (:foreground "dimgray"))))
  (highlight-indent-guides-character-face ((t (:foreground "dimgray")))))

(use-package nerd-icons
  :ensure t
  :defer 1
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-dired
  :ensure t
  :after (nerd-icons)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure t
  :after (nerd-icons)
  :config
  (nerd-icons-completion-mode))

(use-package vundo
  :ensure t
  :if (>= emacs-major-version 28)
  :defer t
  :bind
  (("C-x u" . 'vundo)))

(use-package popper
  :ensure t
  :defer t
  :bind
  (("C-`" . popper-toggle)
   ("C-c k p g" . popper-toggle)
   ("C-c k p f" . popper-cycle)
   ("C-c k p b" . popper-cycle-backwards)
   ("C-c k p t" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*scratch\\*"
          "error\\*$"
          "errors\\*$"
          "\\*xref\\*"
          "\\*Gemini\\*"
          "\\*ChatGPT\\*"
          "^\\*eshell.*" eshell-mode
          "^\\*shell.*" shell-mode
          "^\\*term.*" term-mode
          "^\\*vterm.*" vterm-mode
          "^\\*Python.*" inferior-python-mode
          help-mode
          compilation-mode))
  (setq popper-window-height 20)
  (setq popper-group-function #'popper-group-by-projectile)
  (popper-mode 1)
  (popper-echo-mode 1)
  :after (projectile))

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(setq enable-recursive-minibuffers t)
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)
(when (>= emacs-major-version 28)
  (setq read-extended-command-predicate
        #'command-completion-default-include-p))

(use-package vertico
  :ensure t
  :defer t
  :commands vertico-mode
  :hook
  (after-init . vertico-mode)
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init
  (setq vertico-cycle t)
  (setq vertico-count 12)
  (setq vertico-scroll-margin 4)
  :bind
  (:map vertico-map
        ("RET" . vertico-directory-enter)
        ("DEL" . vertico-directory-delete-char)
        ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode)
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle)))

(use-package embark
  :ensure t
  :defer t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :bind
  (("C-c C-/" . embark-act)
   ("C-c M-/" . embark-dwim)
   ("C-c C-." . embark-export)
   ("C-C M-." . embark-collect)
   ("C-h B" . embark-bindings)))

(use-package consult
  :ensure t
  :defer t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c c r" . consult-recent-file)
         ("C-c c m" . consult-mode-command)
         ("C-c c k" . consult-kmacro)
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
  ;; (setq xref-show-xrefs-function #'consult-xref
  ;;       xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref

   consult--source-buffer
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key "M-."
   ;; :preview-key '(:debounce 0.2 any)
   )
  (setq consult-narrow-key "<")
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package avy
  :ensure t
  :defer 3
  :bind
  (("C-:" . #'avy-goto-char-timer)
   ("C-\"" . #'avy-goto-line)
   ("C-c j :" . #'avy-goto-char-timer)
   ("C-c j \"" . #'avy-goto-line)))

(use-package symbol-overlay
  :ensure t
  :defer 5
  :config
  (setq-default symbol-overlay-map nil)
  :bind
  (("C-;" . #'symbol-overlay-put)
   ("C->" . #'symbol-overlay-jump-next)
   ("C-<" . #'symbol-overlay-jump-prev)
   ("C-c j ;" . #'symbol-overlay-put)
   ("C-c j >" . #'symbol-overlay-jump-next)
   ("C-c j <" . #'symbol-overlay-jump-prev)))

(use-package company
  :ensure t
  :defer 3
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

(use-package treesit-auto
  :if (>= emacs-major-version 29)
  :ensure t
  :defer 1
  :config
  (global-treesit-auto-mode)
  (setq treesit-auto-install 'prompt)
  (delete 'c-sharp treesit-auto-langs))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((js-mode . lsp) (js-ts-mode . lsp) (typescript-mode . lsp) (typescript-ts-mode . lsp)
   (web-mode . lsp) (html-mode . lsp) (css-mode . lsp) (css-ts-mode . lsp) (json-mode . lsp) (json-ts-mode . lsp)
   (python-mode . lsp) (python-ts-mode . lsp)
   (sh-mode . lsp) (bash-ts-mode . lsp)
   (csharp-mode . lsp) (csharp-ts-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :custom
  (lsp-enable-snippet nil)) ;; Stop auto-completing with argument list

(use-package flycheck
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package org
  :defer t
  :init
  (setq-default org-src-fontify-natively t)
  (setq-default org-startup-indented t)
  (setq-default org-pretty-entities t)
  (setq-default org-hide-emphasis-markers t)
  (setq-default org-startup-with-inline-images t)
  (setq-default org-startup-with-latex-preview t)
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
  (setq-default org-export-with-author nil)
  (setq-default org-export-with-date nil)
  (setq-default org-export-with-toc nil)
  (setq-default org-export-with-section-numbers nil)
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
  :config
  (plist-put org-format-latex-options :scale 1.0)
  :bind
  (:map org-mode-map ("C-c C-?" . org-time-stamp-inactive))
  :custom
  (org-list-allow-alphabetical t))

(use-package org-fragtog
  :ensure t
  :hook ((org-mode . org-fragtog-mode))
  :after (org))

(use-package org-roam
  :ensure t
  :defer 2
  :custom
  (org-roam-directory (file-truename "~/Sync/org/roam"))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n j" . org-roam-dailies-capture-today)
   ("C-c n t" . org-roam-tag-add)
   ("C-c n a" . org-roam-alias-add)
   ("C-c n d" . org-id-get-create))
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
      :target (file+head "private/%<%Y-%m-%d>.org.gpg" "#+title: %<%Y-%m-%d>\n")
      :unnarrowed t)
     ("d" "default" entry
      "* %?"
      :target (file+head "daily/%<%Y-%m-%d>.org" "#+filetags: :Personal:Daily:\n#+title: %<%Y-%m-%d>\n")
      :unnarrowed t)
     ("w" "work" entry
      "* %?"
      :target (file+head "work/%<%Y-%m-%d>.org" "#+filetags: :Personal:Work:Daily:\n#+title: %<%Y-%m-%d>\n")
      :unnarrowed t)))
  (setq-default org-roam-node-display-template
                (concat
                 (propertize "${title}" 'face 'bold)
                 (propertize " | " 'face 'org-warning)
                 (propertize "${tags}" 'face 'highlight))))

(use-package org-roam-timestamps
  :ensure t
  :hook ((org-mode . org-roam-timestamps-mode))
  :after (org-roam))

(use-package yasnippet
  :ensure t
  :defer 3
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" default-user-emacs-directory))
  (yas-global-mode)
  :bind (:map yas-minor-mode-map
              ("C-i" . nil) ;; Only <tab> should expand, C-i should not
              ("C-," . 'yas-expand)))

(use-package yasnippet-snippets
  :ensure t
  :defer t
  :after (yasnippet))

(use-package eyebrowse
  :ensure t
  :defer 3
  :config
  (eyebrowse-mode 1))

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  (setq-default projectile-indexing-method 'hybrid)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  :bind
  ("C-c p" . 'projectile-command-map))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-refresh-status-buffer nil)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)
  :bind
  ("C-x g" . 'magit-status))

(use-package git-gutter
  :ensure t
  :defer 3
  :config
  (global-git-gutter-mode 1)
  :bind
  ("C-c k g p" . 'git-gutter:previous-hunk)
  ("C-c k g n" . 'git-gutter:next-hunk)
  ("C-c k g d" . 'git-gutter:popup-hunk)
  ("C-c k g r" . 'git-gutter:revert-hunk))

(use-package auto-virtualenv
  :ensure t
  :config
  (setq auto-virtualenv-verbose nil)
  (setq auto-virtualenv-reload-lsp t)
  (auto-virtualenv-setup))

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(add-hook 'python-mode-hook
          (lambda ()
            (setq python-indent-offset 4)
            (setq tab-width 4)))
(add-hook 'python-ts-mode-hook
          (lambda ()
            (setq python-indent-offset 4)
            (setq tab-width 4)))

(if (>= emacs-major-version 27)
    (add-to-list 'auto-mode-alist '("\\.js[mx]?\\'" . js-mode))
  (add-to-list 'auto-mode-alist '("\\.har\\'" . js-mode)))
(add-hook 'js-mode-hook
          (lambda ()
            (setq-default js-indent-level 2)))
(add-hook 'js-ts-mode-hook
          (lambda ()
            (setq-default js-indent-level 2)))
(with-eval-after-load 'js
  (define-key js-mode-map (kbd "M-.") nil)
  (define-key js-ts-mode-map (kbd "M-.") nil))

(use-package typescript-mode
  :mode "\\.ts$" "\\.tsx$")

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
  (setq-default web-mode-engines-alist '(("django" . "\\.jinja2\\'"))))
(use-package css-mode
  :config
  (setq-default css-indent-offset 2))

(use-package dockerfile-mode
  :ensure t
  :defer t)
(use-package go-mode
  :ensure t
  :defer t)
(use-package json-mode
  :ensure t
  :defer t)
(use-package markdown-mode
  :ensure t
  :defer t)
(use-package php-mode
  :ensure t
  :defer t)
(use-package yaml-mode
  :ensure t
  :defer t)

(use-package exec-path-from-shell
  :ensure t)

(use-package gcmh
  :ensure t
  :defer 1
  :config
  (gcmh-mode 1)
  :custom
  (gcmh-idle-delat 'auto)
  (gcmh-high-cons-threshold 67108864)) ;; 64MB

(use-package rotate
  :ensure t
  :defer 3
  :bind
  (("C-c k r h" . 'rotate:even-horizontal)
   ("C-c k r v" . 'rotate:even-vertical)
   ("C-c k r l" . 'rotate-layout)
   ("C-c k r w" . 'rotate-window)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package gptel
  :ensure t
  :bind
  (("C-c k l g" . #'gptel)
   ("C-c k l s" . #'gptel-send)
   ("C-c k l r" . #'gptel-rewrite)
   ("C-c k l m" . #'gptel-menu)
   ("C-c k l a" . #'gptel-add)
   ("C-c k l f" . #'gptel-add-file))
  :config
  (let ((key-file (expand-file-name "openai-key" user-emacs-directory)))
    (when (file-exists-p key-file)
      (setq gptel-api-key (with-temp-buffer
                            (insert-file-contents key-file)
                            (buffer-string)))))
  (let ((key-file (expand-file-name "gemini-key" user-emacs-directory)))
    (when (file-exists-p key-file)
      (setq
       gptel-model 'gemini-2.0-flash
       gptel-backend (gptel-make-gemini "Gemini"
                       :key (with-temp-buffer
                              (insert-file-contents key-file)
                              (buffer-string))
                       :stream t)))))
