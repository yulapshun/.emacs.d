(defun delete-line (ARG)
"Delete line without saving to `kill-ring'."
  (interactive "P")
  (progn (kill-line ARG)
         (pop kill-ring)
         (pop kill-ring-yank-pointer)))
(defun delete-word (ARG)
"Delete word forward without saving to `kill-ring'."
  (interactive "P")
  (progn (kill-word ARG)
         (pop kill-ring)
         (pop kill-ring-yank-pointer)))
(defun backward-delete-word (ARG)
"Delete word backward without saving to `kill-ring'."
  (interactive "P")
  (or ARG (setq ARG 1))
  (progn (backward-kill-word ARG)
         (pop kill-ring)
         (pop kill-ring-yank-pointer)))

(provide 'defun-delete)
