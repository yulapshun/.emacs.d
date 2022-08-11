;;; cheatsheet.el --- Cheatsheet -*- lexical-binding: t -*-

;;; Commentary:
;; Cheatsheet

;;; Code:

(defvar on99-cheatsheet-directory (expand-file-name "cheatsheets" user-emacs-directory)
  "The directory that contains all the cheatsheets.")

(defun on99-cheatsheet-open-global ()
  "Open global cheatsheet."
  (interactive)
  (find-file-read-only-other-window (expand-file-name "global.org" on99-cheatsheet-directory)))

(defun on99-cheatsheet-open-major-mode ()
  "Open cheatsheet for active major mode."
  (interactive)
  (find-file-read-only-other-window
   (expand-file-name (concat (message "%s" major-mode) ".org") on99-cheatsheet-directory)))

(defun on99-cheatsheet-open-minor-modes ()
  "Open cheatsheet for all active minor modes."
  (interactive))

(provide 'cheatsheet)

;;; cheatsheet.el ends here
