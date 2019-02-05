(require 'lsp)
(require 'lsp-mode)
(require 'lsp-ui)

(custom-set-variables
 '(lsp-auto-guess-root t)
 '(lsp-ui-sideline-enable nil)
 '(lsp-ui-imenu-enable t))

;; Hook javascript modes
;; Install server with the following command, which supports both
;; javascript and typescript
;; npm i -g javascript-typescript-langserver
(add-hook 'js-mode-hook #'lsp)
(add-hook 'js2-mode-hook #'lsp)
(add-hook 'rjsx-mode-hook #'lsp)

;; Hook python modes
;; Install with the following command
;; pip install python-language-server
(add-hook 'python-mode-hook #'lsp)


;; Define key bindings
(let ((map (make-sparse-keymap)))
  (define-key map (kbd "C-c l .") #'lsp-find-definition)
  (define-key map (kbd "C-c l /") #'lsp-find-references)
  (push
   `(lsp-mode . ,map)
   minor-mode-map-alist))
(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

(provide 'init-lsp-mode)
