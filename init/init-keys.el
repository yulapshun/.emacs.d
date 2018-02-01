;; built-in
(global-set-key (kbd "C-x _") 'fit-window-to-buffer)

;; neotree
(global-set-key [f8] 'neotree-toggle)

;; symbol-overlay
(global-set-key (kbd "C-;") 'symbol-overlay-put)
(global-set-key (kbd "C->") 'symbol-overlay-next)
(global-set-key (kbd "C-<") 'symbol-overlay-prev)
(global-set-key [(meta f3)] 'symbol-overlay-query-replace)

;; windmove key bindings
(global-set-key (kbd "C-S-b")  'windmove-left)
(global-set-key (kbd "C-S-f") 'windmove-right)
(global-set-key (kbd "C-S-p")    'windmove-up)
(global-set-key (kbd "C-S-n")  'windmove-down)

;; avy
(global-set-key (kbd "C-:") 'avy-goto-char)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; defun-misc
(global-set-key (kbd "C-\"")  'toggle-column-marker)

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

(provide 'init-keys)
