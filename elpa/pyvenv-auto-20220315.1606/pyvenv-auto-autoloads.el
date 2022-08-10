;;; pyvenv-auto-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pyvenv-auto" "pyvenv-auto.el" (0 0 0 0))
;;; Generated autoloads from pyvenv-auto.el

(defvar pyvenv-auto-mode nil "\
Non-nil if pyvenv-auto mode is enabled.
See the `pyvenv-auto-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `pyvenv-auto-mode'.")

(custom-autoload 'pyvenv-auto-mode "pyvenv-auto" nil)

(autoload 'pyvenv-auto-mode "pyvenv-auto" "\
Turn on pyvenv-auto-mode.

This is a minor mode.  If called interactively, toggle the
`pyvenv-auto mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='pyvenv-auto-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "pyvenv-auto" '("pyvenv-auto-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pyvenv-auto-autoloads.el ends here
