(require 'column-marker)
(defun toggle-column-marker (ARG)
  "Toggle column marker at column ARG
ARG defaults to 79"
  (interactive "P")
  (if (bound-and-true-p column-marker-on)
      (progn
        (column-marker-1 "")
        (setq column-marker-on nil))
    (progn
      (or ARG
          (setq ARG 79))
      (column-marker-1 ARG)
      (setq column-marker-on t))))

(defun eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its value"
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))

(defun control-meta (key)
  "Simulate the behavior of C-M-key."
  (interactive "cC-M-")
  (call-interactively (global-key-binding (kbd (concat "C-M-" (byte-to-string key))))))

(provide 'defun-misc)
