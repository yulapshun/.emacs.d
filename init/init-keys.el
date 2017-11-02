;; neotree
(global-set-key [f8] 'neotree-toggle)

;; highlight
(global-set-key (kbd "C-x y") 'hlt-highlight-symbol)
(global-set-key (kbd "C-x t") 'hlt-unhighlight-symbol)
(global-set-key (kbd "M-p") 'hlt-previous-highlight)
(global-set-key (kbd "M-n") 'hlt-next-highlight)

;; highlight-symbol
(global-set-key (kbd "C-;") 'highlight-symbol)
(global-set-key (kbd "C->") 'highlight-symbol-next)
(global-set-key (kbd "C-<") 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; windmove key bindings
(global-set-key (kbd "C-S-b")  'windmove-left)
(global-set-key (kbd "C-S-f") 'windmove-right)
(global-set-key (kbd "C-S-p")    'windmove-up)
(global-set-key (kbd "C-S-n")  'windmove-down)

;; avy
(global-set-key (kbd "C-:") 'avy-goto-char)

;; defun-misc
(global-set-key (kbd "C-'")  'toggle-column-marker)

;; defun-indent
(global-set-key (kbd "<C-tab>")  'force-indent)
(global-set-key (kbd "<C-S-tab>")  'force-unindent)
(global-set-key (kbd "<C-iso-lefttab>")  'force-unindent)

;; defun-delete
(global-set-key (kbd "C-S-k")  'delete-line)
(global-set-key (kbd "C-S-w")  'delete-region)
(global-set-key (kbd "M-D")  'delete-word)
(global-set-key (kbd "<M-S-backspace>")  'backward-delete-word)

(provide 'init-keys)
