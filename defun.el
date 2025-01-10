;; -*- no-byte-compile: t; lexical-binding: t -*-

(defun my/force-indent-line (ARG)
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

(defun my/force-indent-region (ARG)
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
            (my/force-indent-line ARG)
            (setq i (+ i 1))
            (forward-line))))))

(defun my/force-indent (ARG)
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
  (if mark-active (my/force-indent-region ARG) (my/force-indent-line ARG)))

(defun my/force-unindent-line (ARG)
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

(defun my/force-unindent-region (ARG)
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
            (my/force-unindent-line ARG)
            (setq i (+ i 1))
            (forward-line))))))

(defun my/force-unindent (ARG)
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
  (if mark-active (my/force-unindent-region ARG) (my/force-unindent-line ARG)))

(defun my/delete-line (ARG)
"Delete line without saving to `kill-ring'."
  (interactive "P")
  (progn (kill-line ARG)
         (pop kill-ring)
         (pop kill-ring-yank-pointer)))
(defun my/delete-word (ARG)
"Delete word forward without saving to `kill-ring'."
  (interactive "P")
  (progn (kill-word ARG)
         (pop kill-ring)
         (pop kill-ring-yank-pointer)))
(defun my/backward-delete-word (ARG)
"Delete word backward without saving to `kill-ring'."
  (interactive "P")
  (or ARG (setq ARG 1))
  (progn (backward-kill-word ARG)
         (pop kill-ring)
         (pop kill-ring-yank-pointer)))

(defun my/control (key)
  "Simulate the behavior of C-key."
  (interactive "cC-")
  (call-interactively (global-key-binding (kbd (concat "C-" (byte-to-string key))))))

(defun my/meta (key)
  "Simulate the behavior of M-key."
  (interactive "cM-")
  (call-interactively (global-key-binding (kbd (concat "M-" (byte-to-string key))))))

(defun my/control-shift (key)
  "Simulate the behavior of C-S-key."
  (interactive "cC-S-")
  (call-interactively (global-key-binding (kbd (concat "C-S-" (byte-to-string key))))))

(defun my/meta-shift (key)
  "Simulate the behavior of M-S-key."
  (interactive "cM-S-")
  (call-interactively (global-key-binding (kbd (concat "M-S-" (byte-to-string key))))))

(defun my/control-meta (key)
  "Simulate the behavior of C-M-key."
  (interactive "cC-M-")
  (call-interactively (global-key-binding (kbd (concat "C-M-" (byte-to-string key))))))

(defun my/eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its value"
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))

(defun my/sudo-edit (&optional arg)
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
