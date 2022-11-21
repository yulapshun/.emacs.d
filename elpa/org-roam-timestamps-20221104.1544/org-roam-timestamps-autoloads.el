;;; org-roam-timestamps-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-roam-timestamps" "org-roam-timestamps.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-roam-timestamps.el

(defvar org-roam-timestamps-mode nil "\
Non-nil if org-roam-timestamps mode is enabled.
See the `org-roam-timestamps-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-roam-timestamps-mode'.")

(custom-autoload 'org-roam-timestamps-mode "org-roam-timestamps" nil)

(autoload 'org-roam-timestamps-mode "org-roam-timestamps" "\
Automatically add creation and modification timestamps to org-roam nodes.

This is a minor mode.  If called interactively, toggle the
`org-roam-timestamps mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='org-roam-timestamps-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "org-roam-timestamps" '("org-roam-timestamps-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-roam-timestamps-autoloads.el ends here
