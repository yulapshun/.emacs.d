(require 'column-marker)
(defun toggle-column-marker (ARG)
  "Toggle column marker at column ARG
ARG defaults to 80"
  (interactive "P")
  (if (bound-and-true-p column-marker-on)
      (progn
        (column-marker-1 "")
        (setq column-marker-on nil))
    (progn
      (or ARG
          (setq ARG 80))
      (column-marker-1 ARG)
      (setq column-marker-on t))))

(defun eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its value"
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (if ido-mode
                             (ido-read-file-name "Find file(as root): "))
                         (read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


(provide 'defun-misc)
