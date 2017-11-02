(defun force-indent-line (ARG)
  "Force indent current line.
If `indent-tabs-mode' is enabled, insert ARG tab at the begining of line,
else insert a number of spaces specified by `tab-width' times ARG at the
beginning of line."
  (or ARG (setq ARG 1))
  (save-excursion
    (move-beginning-of-line 1)
    (if indent-tabs-mode
        (insert (make-string ARG ?\t))
      (insert (make-string (* ARG tab-width) ?\s)))))

(defun force-indent-region (ARG)
  "Force indent every line in the active region.
If `indent-tabs-mode' is enabled, insert ARG tab at the begining of every line
in the active region, else insert a number of spaces specified by `tab-width'
times ARG at the beginning of every line in the active region."
  (or ARG (setq ARG 1))
  (if mark-active
      (let ((line-count (count-lines (region-beginning) (region-end)))
            (i 0))
        (save-excursion
          (goto-char (region-beginning))
          (while (< i line-count)
            (force-indent-line ARG)
            (setq i (+ i 1))
            (forward-line))))))

(defun force-indent (ARG)
  "Force indent current line or active region.

If region is inactive:
If `indent-tabs-mode' is enabled, insert ARG tab at the begining of line,
else insert a number of spaces specified by `tab-width' times ARG at the
beginning of line.

If region is active
If `indent-tabs-mode' is enabled, insert ARG tab at the begining of every line
in the active region, else insert a number of spaces specified by `tab-width'
times ARG at the beginning of every line in the active region."
  (interactive "P")
  (or ARG (setq ARG 1))
  (if mark-active (force-indent-region ARG) (force-indent-line ARG)))

(defun force-unindent-line (ARG)
  "Force unindent current line.
If `indent-tabs-mode' is enabled, delete ARG tab character at the begining of
line, else delete at most a number of spaces specified by `tab-width'
times ARG at the beginning of line."
  (or ARG (setq ARG 1))
  (save-excursion
    (move-beginning-of-line 1)
    (setq start (point))
    (if indent-tabs-mode
        (progn
          (skip-chars-forward "\t")
          (if (< (- (point) start) ARG)
              (delete-horizontal-space)
            (delete-backward-char ARG)))
      (progn
          (skip-chars-forward " ")
          (if (< (- (point) start) (* ARG tab-width))
              (delete-horizontal-space)
            (delete-backward-char (* ARG tab-width)))))))

(defun force-unindent-region (ARG)
  "Force unindent every line in the active region.
If `indent-tabs-mode' is enabled, delete ARG tab character at the begining of
every line in the active region, else delete at most a number of spaces
specified by `tab-width' times ARG at the beginning of every line in the active region."
  (or ARG (setq ARG 1))
  (if mark-active
      (let ((line-count (count-lines (region-beginning) (region-end)))
            (i 0))
        (save-excursion
          (goto-char (region-beginning))
          (while (< i line-count)
            (force-unindent-line ARG)
            (setq i (+ i 1))
            (forward-line))))))

(defun force-unindent (ARG)
  "Force unindent current line or active region.

If region is inactive:
If `indent-tabs-mode' is enabled, delete ARG tab character at the begining of
line, else delete at most a number of spaces specified by `tab-width'
times ARG at the beginning of line

If region is active
If `indent-tabs-mode' is enabled, delete ARG tab character at the begining of
every line in the active region, else delete at most a number of spaces
specified by `tab-width' times ARG at the beginning of every line in the active region."
  (interactive "P")
  (or ARG (setq ARG 1))
  (if mark-active (force-unindent-region ARG) (force-unindent-line ARG)))

(provide 'defun-indent)
