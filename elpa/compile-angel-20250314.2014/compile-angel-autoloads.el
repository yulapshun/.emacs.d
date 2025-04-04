;;; compile-angel-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from compile-angel.el

(defvar compile-angel-on-load-mode nil "\
Non-nil if Compile-Angel-On-Load mode is enabled.
See the `compile-angel-on-load-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `compile-angel-on-load-mode'.")
(custom-autoload 'compile-angel-on-load-mode "compile-angel" nil)
(autoload 'compile-angel-on-load-mode "compile-angel" "\
Toggle `compile-angel-mode' then compiles .el files before they are loaded.

This is a global minor mode.  If called interactively, toggle the
`Compile-Angel-On-Load mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='compile-angel-on-load-mode)'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(defvar compile-angel-on-save-mode nil "\
Non-nil if Compile-Angel-On-Save mode is enabled.
See the `compile-angel-on-save-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `compile-angel-on-save-mode'.")
(custom-autoload 'compile-angel-on-save-mode "compile-angel" nil)
(autoload 'compile-angel-on-save-mode "compile-angel" "\
Toggle `compile-angel-mode'that compiles .el file when saved.

This is a global minor mode.  If called interactively, toggle the
`Compile-Angel-On-Save mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='compile-angel-on-save-mode)'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(autoload 'compile-angel-on-save-local-mode "compile-angel" "\
Toggle `compile-angel-mode'that compiles .el file when saved.

This is a minor mode.  If called interactively, toggle the
`Compile-Angel-On-Save-Local mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable the
mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate the variable `compile-angel-on-save-local-mode'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "compile-angel" '("compile-angel-"))

;;; End of scraped data

(provide 'compile-angel-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; compile-angel-autoloads.el ends here
