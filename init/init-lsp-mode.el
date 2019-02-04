(require 'lsp-mode)

(setq lsp-auto-guess-root t)

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

(provide 'init-lsp-mode)
