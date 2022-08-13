;;; fast-init.el --- Fast emcas start up -*- lexical-binding: t -*-

;;; Commentary:
;; Set fast-init to true some configs are ignored to speedup startup time
;; Run with `emacs -q -l ~/.emacs.d/fast-init.el --no-splash -nw`

;;; Code:

(defvar fast-init t)

(load-file (expand-file-name "init.el" user-emacs-directory))

(run-hooks 'after-init-hook)

(provide 'fast-init)

;;; fast-init.el ends here
