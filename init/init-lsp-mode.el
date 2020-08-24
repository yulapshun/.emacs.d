(require 'lsp)
(require 'lsp-mode)
(require 'lsp-ui)

;; Tuning
(setq gc-cons-threshold 100000000)
(setq read-process-output-max 1048576) 

(custom-set-variables
 '(lsp-auto-guess-root nil)
 '(lsp-ui-sideline-enable nil)
 '(lsp-ui-imenu-enable t)
 '(lsp-ui-peek-always-show t)
 '(lsp-ui-doc-enable nil))

;; npm i -g typescript-language-server
;; pip install python-language-server
;; npm i -g vscode-css-languageserver-bin
;; npm i -g vscode-html-languageserver-bin

(add-hook 'js-mode-hook #'lsp)
(add-hook 'js2-mode-hook #'lsp)
(add-hook 'rjsx-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'web-mode-hook #'lsp)
(add-hook 'css-mode-hook #'lsp)


;; Define key bindings
(let ((map (make-sparse-keymap)))
  (define-key map (kbd "C-c l .") #'lsp-find-definition)
  (define-key map (kbd "C-c l /") #'lsp-find-references)
  (define-key map (kbd "C-c l d") #'lsp-ui-doc-glance)
  (define-key map (kbd "C-c l i") #'lsp-ui-peek-find-implementation)
  (define-key map (kbd "C-c l I") #'lsp-find-implementation)
  (push
   `(lsp-mode . ,map)
   minor-mode-map-alist))
(define-key lsp-ui-mode-map [remap js-find-symbol] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

(provide 'init-lsp-mode)
