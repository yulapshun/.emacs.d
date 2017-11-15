(defun hihi-control (key)
  "Simulate the behavior of C-M-key."
  (interactive "cC-")
  (call-interactively (global-key-binding (kbd (concat "C-" (byte-to-string key))))))

(defun hihi-meta (key)
  "Simulate the behavior of C-M-key."
  (interactive "cM-")
  (call-interactively (global-key-binding (kbd (concat "M-" (byte-to-string key))))))

(defun hihi-control-shift (key)
  "Simulate the behavior of C-M-key."
  (interactive "cC-S-")
  (call-interactively (global-key-binding (kbd (concat "C-S-" (byte-to-string key))))))

(defun hihi-meta-shift (key)
  "Simulate the behavior of C-M-key."
  (interactive "cM-S-")
  (call-interactively (global-key-binding (kbd (concat "M-S-" (byte-to-string key))))))

(defun hihi-control-meta (key)
  "Simulate the behavior of C-M-key."
  (interactive "cC-M-")
  (call-interactively (global-key-binding (kbd (concat "C-M-" (byte-to-string key))))))

(provide 'defun-modifier)
