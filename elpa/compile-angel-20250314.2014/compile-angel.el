;;; compile-angel.el --- Automatically Compile Elisp files (auto-compile alternative) -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Package-Version: 20250314.2014
;; Package-Revision: ea7c270a6f56
;; URL: https://github.com/jamescherti/compile-angel.el
;; Keywords: convenience
;; Package-Requires: ((emacs "26.3"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; The compile-angel package automatically byte-compiles and native-compiles
;; Emacs Lisp libraries. It offers:
;; - (compile-angel-on-load-mode): A global mode that compiles .el files before
;;   they are loaded.
;; - (compile-angel-on-save-local-mode): A local mode that compiles .el files
;;   whenever the user saves them.
;;
;; The compile-angel modes speed up Emacs by ensuring all libraries are
;; byte-compiled and native-compiled. Byte-compilation reduces the overhead of
;; loading Emacs Lisp code at runtime, while native compilation optimizes
;; performance by generating machine code specific to your system.
;;
;; The author of compile-angel was previously a user of auto-compile but
;; encountered an issue where several .el files were not being compiled by
;; auto-compile, resulting in Emacs performance degradation due to the lack of
;; native compilation. After extensive experimentation and research, the author
;; developed compile-angel to address this problem. The compile-angel package
;; guarantees that all .el files are both byte-compiled and native-compiled,
;; which significantly speeds up Emacs.
;;
;; The compile-angel package was created to offer an alternative to auto-compile
;; that guarantees all .el files are both byte-compiled and native-compiled,
;; which significantly speeds up Emacs.
;;
;; Before installing:
;; ------------------
;; It is highly recommended to set the following variables in your init file:
;;   (setq load-prefer-newer t)
;;   (setq native-comp-jit-compilation t)
;;   (setq native-comp-deferred-compilation t) ; Deprecated in Emacs > 29.1
;;
;; Additionally, ensure that native compilation is enabled; this should
;; return t: `(native-comp-available-p)`.
;;
;; Installation from MELPA:
;; ------------------------
;; (use-package compile-angel
;;   :ensure t
;;   :demand t
;;   :custom
;;   (compile-angel-verbose t)
;;
;;   :config
;;   ;; Set `compile-angel-verbose' to nil to disable compile-angel messages.
;;   ;; (When nil, compile-angel won't show which file is being compiled.)
;;   (setq compile-angel-verbose t)
;;
;;   ;; Uncomment the line below to auto compile when an .el file is saved
;;   ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)
;;
;;   (compile-angel-on-load-mode))
;;
;; Links:
;; ------
;; - More information about compile-angel (Frequently asked questions, usage...):
;;   https://github.com/jamescherti/compile-angel.el

;;; Code:

(require 'bytecomp)
(require 'cl-lib)
(eval-when-compile (require 'subr-x))

;;; Variables

(defgroup compile-angel nil
  "Compile Emacs Lisp libraries automatically."
  :group 'compile-angel
  :prefix "compile-angel-")

(defcustom compile-angel-enable-byte-compile t
  "Non-nil to enable byte compilation of Emacs Lisp (.el) files."
  :type 'boolean
  :group 'compile-angel)

(defcustom compile-angel-enable-native-compile t
  "Non-nil to enable native-compilation of Emacs Lisp (.el) files."
  :type 'boolean
  :group 'compile-angel)

(defcustom compile-angel-excluded-files '("loaddefs.el"
                                          "autoloads.el"
                                          ;; Built-in no-byte-compile packages
                                          "/charprop.el"
                                          "/cus-load.el"
                                          "/finder-inf.el")
  "A list of path suffixes used to exclude specific .el files from compilation.

Example: \\='(\"suffix.el\" \"/filename.el\") This excludes any path that ends
with \"suffix.el\" (or its variations, such as \"/path/ANYTHINGsuffix.el.gz\")
and exactly matches paths that end with \"/filename.el\" (including their
variations, like \"/filename.el.gz\" or \"ANYTHING/filename.el.gz\").

If a path suffix in `compile-angel-excluded-files' ends with .el, compile-angel
will automatically exclude the .el.gz variant of that file as well (e.g.,
\"suffix.el\" will also exclude \"ANYTHINGsuffix.el.gz\").

The variable `load-file-rep-suffixes' is used by compile-angel to detect and
include all extensions associated with .el files."
  :type '(repeat string)
  :group 'compile-angel)

(defcustom compile-angel-excluded-files-regexps nil
  "A list of regular expressions to exclude certain .el files from compilation.

It is advisable to use `compile-angel-excluded-files' instead of
`compile-angel-excluded-files-regexps', as it simplifies matching file names.
Regular expressions may become unnecessarily complex in this context,
particularly since .el files might also end with the extension .el.gz on certain
configurations. Furthermore, Emacs regular expressions differ from PCRE, adding
another layer of potential complexity.

These regular expression apply to all modes:
- `compile-angel-on-load-mode'
- `compile-angel-on-save-local-mode'"
  :type '(repeat string)
  :group 'compile-angel)

(defcustom compile-angel-verbose nil
  "Enable displaying messages (e.g., when files are compiled).
When set to non-nil, this option will cause messages to be shown during the
compilation process, providing feedback on the compilation status."
  :type 'boolean
  :group 'compile-angel)

(defcustom compile-angel-debug nil
  "Non-nil to display debug messages in the *compile-angel:debug* buffer.
This displays a lot of messages."
  :type 'boolean
  :group 'compile-angel)

(defcustom compile-angel-byte-compile-report-issues nil
  "Controls whether byte-compilation warnings are reported.

When set to nil, `byte-compile-file' will disregard the value of
`byte-compile-warnings', preventing byte-compilation warnings from being shown
to the user.

When set to a non-nil value, this variable ensures that the value of
`byte-compile-warnings' is respected during byte-compilation and warnings are
displayed accordingly."
  :type 'boolean
  :group 'compile-angel)

(defcustom compile-angel-predicate-function nil
  "Function that determines if an .el file should be compiled.
It takes one argument (an .el file) and returns t if the file should be
compiled, or nil if the file should not be compiled.

This function applies to all modes:
- `compile-angel-on-load-mode'
- `compile-angel-on-save-local-mode'"
  :group 'compile-angel
  :type '(choice (const nil)
                 (function)))

(defcustom compile-angel-on-load-mode-compile-once t
  "If non-nil, enable single compilation for `compile-angel-on-load-mode'.
This setting ensures that `compile-angel-on-load-mode' performs byte and native
compilation of .el files only once during the initial loading. When set to nil,
the mode will verify if the .el file needs recompilation each time it is
loaded."
  :type 'boolean
  :group 'compile-angel)

;; Enable/Disable features
(defvar compile-angel-on-load-advise-load t
  "When non-nil, automatically compile .el files loaded using `load'.")

(defvar compile-angel-on-load-advise-require t
  "When non-nil, automatically compile .el files loaded using `require'.")

(defvar compile-angel-on-load-hook-after-load-functions t
  "Non-nil to compile missed .el during `after-load-functions'.")

(defvar compile-angel-on-load-compile-features t
  "Non-nil to compile features listed in the `features' variable.
When `compile-angel-on-load-mode' is activated, this ensures that all features
listed in the `features' variable are compiled.")

(defvar compile-angel-on-load-compile-load-history t
  "Non-nil to compile all uncompiled files in the load history.
This ensures that all files loaded before `compile-angel-on-load-mode' is
activated are compiled when this mode is activated.")

(defcustom compile-angel-on-save-check-parens t
  "Non-nil to check for unbalanced parentheses before compiling a file.
This applies only to `compile-angel-on-save-local-mode' and
`compile-angel-on-save-mode'. If unbalanced parentheses are detected, the file
is not compiled, as the compilation would fail anyway."
  :group 'compile-angel
  :type 'boolean)

;;; Internal variables

(defvar compile-angel--quiet-byte-compile-file t)
(defvar compile-angel--list-compiled-files (make-hash-table :test 'equal))
(defvar compile-angel--currently-compiling nil)
(defvar compile-angel--force-compilation nil)
(defvar compile-angel--el-file-regexp nil)
(defvar compile-angel--el-file-extensions nil)
(defvar compile-angel--excluded-path-suffixes-regexps nil)

;;; Functions

(defun compile-angel--insert-message (buffer-name msg &rest args)
  "Insert formatted MSG with ARGS into BUFFER-NAME buffer."
  (let ((inhibit-read-only t))
    (with-current-buffer (get-buffer-create buffer-name)
      (goto-char (point-max))
      (insert (apply 'format msg args) "\n"))))

(defmacro compile-angel--debug-message (&rest args)
  "Display a debug message with the same ARGS arguments as `message'.
The messages are displayed in the *compile-angel* buffer."
  `(when compile-angel-debug
     (compile-angel--insert-message "*compile-angel:debug*"
                                    ,(car args) ,@(cdr args))))

(defmacro compile-angel--verbose-message (&rest args)
  "Display a verbose message with the same ARGS arguments as `message'."
  `(progn
     (when compile-angel-debug
       (compile-angel--debug-message ,(car args) ,@(cdr args)))
     (when compile-angel-verbose
       (message (concat "[compile-angel] " ,(car args)) ,@(cdr args)))))

(defun compile-angel--el-file-excluded-p (el-file)
  "Check if EL-FILE matches any regex in `compile-angel-excluded-files-regexps'.
Return non-nil if the file should be ignored, nil otherwise."
  (when (or (and compile-angel--excluded-path-suffixes-regexps
                 (cl-some (lambda (regex)
                            (string-match-p regex el-file))
                          compile-angel--excluded-path-suffixes-regexps))
            (and compile-angel-excluded-files-regexps
                 (cl-some (lambda (regex)
                            (string-match-p regex el-file))
                          compile-angel-excluded-files-regexps)))
    (compile-angel--debug-message
     "SKIP (.el file excluded with a regex): %s" el-file)
    t))

(defun compile-angel--elisp-native-compiled-p (el-file)
  "Return non-nil if EL-FILE is native-compiled and up to date.
Return nil if it is not native-compiled or if its .eln file is out of date."
  (let ((eln-file (and (fboundp 'comp-el-to-eln-filename)
                       (funcall 'comp-el-to-eln-filename el-file))))
    (when (and eln-file
               (file-exists-p eln-file)
               (file-newer-than-file-p eln-file el-file))
      t)))

(defun compile-angel--native-compile (el-file)
  "Native-compile EL-FILE."
  (cond (t
         (cond
          ((not (and (featurep 'native-compile)
                     (fboundp 'native-comp-available-p)
                     (fboundp 'native-compile-async)
                     (native-comp-available-p)))
           (compile-angel--debug-message
            "Native-compilation ignored (native-comp unavailable): %s" el-file))

          ((compile-angel--elisp-native-compiled-p el-file)
           (compile-angel--debug-message
            "Native-compilation ignored (up-to-date): %s" el-file))

          (t
           (let ((el-file-abbreviated (abbreviate-file-name el-file)))
             (compile-angel--verbose-message
              "Async native-compilation: %s" el-file-abbreviated))
           (let ((inhibit-message (not (or (not compile-angel-verbose)
                                           (not compile-angel-debug)))))
             ;; TODO: Move featurep before the first (cond in this function
             (when (and (featurep 'native-compile)
                        (fboundp 'native-compile-async))
               (funcall 'native-compile-async el-file))))))))

(defun compile-angel--byte-compile (el-file elc-file)
  "Byte-compile EL-FILE into ELC-FILE.
Return non-nil to allow native compilation."
  (compile-angel--debug-message "Start: Byte-compilation: %s -> %s"
                                el-file elc-file)
  (cond
   ((and (file-exists-p elc-file)
         (not (file-newer-than-file-p el-file elc-file)))
    (compile-angel--debug-message
     "Byte-compilation Ignored (up-to-date): %s" el-file)
    t)

   (t
    (let* ((byte-compile-warnings
            (when compile-angel-byte-compile-report-issues
              byte-compile-warnings))
           (after-change-major-mode-hook
            (and (fboundp 'global-font-lock-mode-enable-in-buffer)
                 (list 'global-font-lock-mode-enable-in-buffer)))
           (inhibit-message (not (or (not compile-angel-verbose)
                                     (not compile-angel-debug))))
           (el-file-abbreviated (abbreviate-file-name el-file))
           (prog-mode-hook nil)
           (emacs-lisp-mode-hook nil)
           (byte-compile-result
            (let ((original-message (symbol-function 'message)))
              (cl-letf (((symbol-function #'message)
                         #'(lambda (format-string &rest messages-args)
                             (let ((combined-args (cons format-string
                                                        messages-args)))
                               (when (or (not compile-angel--quiet-byte-compile-file)
                                         (and compile-angel--quiet-byte-compile-file
                                              (and (not (string-prefix-p "Wrote" format-string))
                                                   (not (string-prefix-p "Compiling " format-string)))))
                                 ;; Show the message
                                 (apply original-message combined-args))))))
                (byte-compile-file el-file)))))
      (cond
       ((eq byte-compile-result 'no-byte-compile)
        (compile-angel--debug-message
         "Byte-compilation Ignore (no-byte-compile): %s" el-file-abbreviated)
        nil)

       ((not byte-compile-result)
        (compile-angel--verbose-message "Byte-compilation error: %s"
                                        el-file-abbreviated)
        nil)

       (byte-compile-result
        (compile-angel--verbose-message
         "Byte-compilation successful: %s" el-file-abbreviated)
        t))))))

(defun compile-angel--need-compilation-p (el-file feature-name)
  "Return non-nil if EL-FILE or FEATURE-NAME need compilation.
EL-FILE is a String representing the path to the Elisp source file.
FEATURE-NAME is a string representing the feature name being loaded."
  (cond
   ((not el-file)
    (compile-angel--debug-message
     "SKIP (el-file is nil): %s | %s" el-file feature-name)
    nil)

   ((not (compile-angel--is-el-file el-file))
    (compile-angel--debug-message
     "SKIP (Does not end with the .el): %s | %s" el-file feature-name)
    nil)

   ((and (not compile-angel--force-compilation)
         (and compile-angel-on-load-mode-compile-once
              (gethash el-file compile-angel--list-compiled-files)))
    (compile-angel--debug-message
     "SKIP (In the skip hash list): %s | %s" el-file feature-name)
    nil)

   (t
    (let ((decision (if compile-angel-predicate-function
                        (funcall compile-angel-predicate-function el-file)
                      t)))
      (when (and (not (eq decision t))  ; t = :continue
                 (not (eq decision nil))  ; nil = :ignore

                 ;; `:compile' have precedence over excluded files
                 ;; (`compile-angel-excluded-files' and
                 ;; `compile-angel-excluded-files-regexps')
                 (not (eq decision :compile))

                 ;; `:continue' is the same as nil
                 (not (eq decision :continue))

                 ;; Do not use `:force-compile'. Use `:compile' instead.
                 (not (eq decision :force-compile))

                 ;; Do not compile the file
                 (not (eq decision :ignore)))
        (message (format
                  (concat "[compile-angel] WARNING: The predicate function "
                          "`compile-angel-predicate-function' returned an "
                          "invalid value: %s")
                  decision))
        (setq decision t))
      (cond
       ((eq decision :force-compile)
        t)

       ;; This is specific to Doom Emacs: It ensures that the Doom Emacs user
       ;; directory, Emacs directory, and modules directory are not compiled by
       ;; compile-angel. This is important because `.el` files in these
       ;; directories should never be compiled, or Doom may fail to load some of
       ;; them correctly.
       ((or (and (boundp 'doom-user-dir)
                 (file-in-directory-p el-file doom-user-dir))
            (and (boundp 'doom-emacs-dir)
                 (file-in-directory-p
                  el-file (expand-file-name "lisp" doom-emacs-dir)))
            (and (boundp 'doom-modules-dir)
                 (file-in-directory-p
                  el-file (expand-file-name doom-modules-dir))))
        (compile-angel--debug-message
         "SKIP (Doom Emacs modules/emacs/user directory): %s | %s"
         el-file feature-name)
        nil)

       ((eq decision :compile)
        t)

       ((eq decision :ignore)
        (compile-angel--debug-message
         "SKIP (Predicate function returned :ignore): %s | %s"
         el-file feature-name)
        nil)

       ((not decision)  ; nil = :ignore
        (compile-angel--debug-message
         "SKIP (Predicate function returned nil): %s | %s" el-file feature-name)
        nil)

       ;; :continue starts here:
       ((compile-angel--el-file-excluded-p el-file)
        nil)

       (t t))))))

(defun compile-angel--compile-elisp (el-file)
  "Byte-compile and Native-compile the .el file EL-FILE."
  (let* ((elc-file (byte-compile-dest-file el-file)))
    (cond
     ((not (file-exists-p el-file))
      (message "[compile-angel] Warning: The file does not exist: %s" el-file))

     ((not elc-file)
      (message "[compile-angel] Warning: The file is not an .el file: %s"
               el-file))

     (t
      (let ((elc-writable (file-writable-p elc-file))
            (do-native-compile nil))
        (if (not compile-angel-enable-byte-compile)
            ;; Byte-compile Disabled
            (when compile-angel-enable-native-compile
              (compile-angel--debug-message
               "Native-compilation only: %s" el-file)
              (setq do-native-compile t))
          ;; Byte-compile enabled
          (cond
           (elc-writable
            ;; Byte-compile
            ;; (compile-angel--debug-message
            ;;  "[compile-angel] Byte %scompilation: %s"
            ;;  (when compile-angel-enable-native-compile "and Native " "")
            ;;  el-file)
            (setq do-native-compile (compile-angel--byte-compile
                                     el-file elc-file)))

           ;; .elc not writable
           (t
            ;; Do not byte-compile
            (compile-angel--debug-message
             "Byte-compilation ignored (not writable)%s: %s"
             (if compile-angel-enable-native-compile
                 ". Native-compilation only"
               "")
             elc-file)
            (setq do-native-compile t))))

        (when do-native-compile
          ;; When the .elc is not writable, force native compilation even when
          ;; JIT is enabled.
          (compile-angel--native-compile el-file)))))))

(defun compile-angel--check-parens ()
  "Check for unbalanced parentheses in the current buffer.

This function scans the entire buffer for balanced parentheses. If an imbalance
is found, it raises a user error with details about the position of the
unmatched parenthesis or quote, including the line number, column number, and
the file name.

If the parentheses are balanced, it returns t. If unbalanced parentheses are
detected, it raises an error and returns nil."
  (interactive)
  (condition-case data
      (progn
        (scan-sexps (point-min) (point-max))
        ;; Return t
        t)
    (scan-error
     (let ((char (nth 2 data)))
       (user-error
        (concat "[compile-angel] Compilation aborted: Unmatched bracket or "
                "quote in line %s, column %s in %s")
        (line-number-at-pos char)
        (let ((column (save-excursion (goto-char char) (current-column))))
          (when (integerp column)
            (1+ column)))
        (let ((file-name (buffer-file-name (buffer-base-buffer))))
          (when file-name
            (abbreviate-file-name file-name))))
       ;; Return nil
       nil))))

(defun compile-angel--compile-on-save ()
  "Compile the current buffer."
  (when (derived-mode-p 'emacs-lisp-mode)
    (let ((el-file (buffer-file-name (buffer-base-buffer)))
          (compile-angel--force-compilation t))
      (when (or (not compile-angel-on-save-check-parens)
                (compile-angel--check-parens))
        (compile-angel--entry-point el-file)))))

(defun compile-angel--feature-to-feature-name (feature)
  "Convert a FEATURE symbol into a feature name and return it."
  (cond
   ((stringp feature)
    feature)
   ((symbolp feature)
    (symbol-name feature))
   (t
    (compile-angel--debug-message
     "ISSUE: UNSUPPORTED Feature: Not a symbol: %s (type: %s)"
     feature (type-of feature))
    nil)))

(defun compile-angel--guess-el-file (el-file
                                     &optional feature-name nosuffix)
  "Guess the path of the EL-FILE or FEATURE-NAME.

EL-FILE must be an absolute path. If EL-FILE is not provided,
FEATURE-NAME is used to search.

NOSUFFIX behaves similarly to `load', controlling whether file suffixes are
considered. Checks caches before performing any computation. Returns the
resolved file path or nil if not found."
  (let* ((el-file (when (and (stringp el-file)
                             (file-name-absolute-p el-file))
                    el-file))
         (result nil))
    ;; Return result
    (if result
        result
      ;; Find result and return it
      (setq result (if (and el-file
                            (compile-angel--is-el-file el-file))
                       el-file
                     (locate-file (or el-file feature-name)
                                  load-path
                                  (if nosuffix
                                      load-file-rep-suffixes
                                    compile-angel--el-file-extensions))))
      result)))

(defun compile-angel--entry-point (el-file &optional feature nosuffix)
  "This function is called by all the :before advices.
EL-FILE, FEATURE, and NOSUFFIX are the same arguments as `load' and `require'."
  (when (or compile-angel-enable-byte-compile
            compile-angel-enable-native-compile)
    (let* ((feature-name (compile-angel--feature-to-feature-name feature))
           (el-file (compile-angel--guess-el-file
                     el-file feature-name nosuffix)))
      (compile-angel--debug-message "COMPILATION ARGS: %s | %s"
                                    el-file feature-name)
      (cond
       ((not el-file)
        (compile-angel--debug-message
         "SKIP (Returned a nil .el file): %s | %s" el-file feature))

       ((member el-file compile-angel--currently-compiling)
        (compile-angel--debug-message
         "SKIP (To prevent recursive compilation): %s | %s" el-file feature))

       ((not (compile-angel--need-compilation-p el-file feature-name))
        (compile-angel--debug-message
         "SKIP (Does not need compilation): %s | %s" el-file feature))

       (t
        (puthash el-file t compile-angel--list-compiled-files)
        (let ((compile-angel--currently-compiling
               (cons el-file compile-angel--currently-compiling)))
          (compile-angel--compile-elisp el-file)))))))

(defun compile-angel--advice-before-require (feature
                                             &optional filename _noerror)
  "Recompile the library before `require'.
FEATURE and FILENAME are the same arguments as the `require' function."
  (if (and compile-angel-on-load-mode-compile-once
           (featurep feature))
      (compile-angel--debug-message
       "SKIP: REQUIRE (Feature already provided): %s (%s) | %s (%s)"
       filename (type-of filename) feature (type-of feature))
    (compile-angel--debug-message
     "REQUIRE: %s (%s) | %s (%s)"
     filename (type-of filename) feature (type-of feature))
    (compile-angel--entry-point filename feature)))

(defun compile-angel--advice-before-load (el-file &optional _noerror _nomessage
                                                  nosuffix _must-suffix)
  "Recompile before `load'. EL-FILE and NOSUFFIX are the same args as `load'."
  (compile-angel--debug-message "LOAD: %s (%s)" el-file (type-of el-file))
  (if (stringp el-file)
      ;; Unset the special init-file status to prevent recursive loads
      (let ((user-init-file (if (eq user-init-file t)
                                nil
                              user-init-file)))
        (compile-angel--entry-point (when el-file
                                      (expand-file-name
                                       (substitute-in-file-name el-file)))
                                    nil nosuffix))
    (compile-angel--debug-message
     (concat "ISSUE: Wrong type passed to "
             "compile-angel--advice-before-require %s (%s)")
     el-file (type-of el-file))))

(defun compile-angel--compile-load-history ()
  "Compile `load-history', which tracks all previously loaded files."
  (dolist (entry load-history)
    (let ((fname (car entry)))
      (when (compile-angel--is-el-file fname)
        (progn
          (compile-angel--debug-message
           "compile-angel--compile-load-history: %s" fname)
          (compile-angel--entry-point fname))))))

(defun compile-angel--compile-features ()
  "Compile all loaded features that are in the `features' variable."
  (dolist (feature features)
    (compile-angel--debug-message
     "compile-angel-compile-features: %s" feature)
    (compile-angel--entry-point nil feature)))

(defun compile-angel--find-el-file (file)
  "Find the .el file corresponding to FILE.

If FILE is already a .el file, return it. If FILE is a .elc file, check for the
corresponding .el file by removing the .elc extension and verifying its
existence.

The function iterates through the extensions in `load-file-rep-suffixes` to
construct possible .el file paths. If a matching file exists, return its path;
otherwise, return nil."
  (cond
   ((not file)
    (compile-angel--debug-message
     "compile-angel--find-el-file: nil file")
    nil)

   ((compile-angel--is-el-file file)
    file)

   ((string-equal (file-name-extension file) "elc")
    (let ((base (file-name-sans-extension file))
          (suffixes load-file-rep-suffixes)
          result)
      (while (and suffixes (not result))
        (let ((candidate (concat base ".el")))
          (when (file-exists-p candidate)
            (setq result candidate)))
        (setq suffixes (cdr suffixes)))
      result))

   (t
    (compile-angel--debug-message
     "compile-angel--find-el-file: NO .el FILE CORRESPONDS TO: %s" file))))

(defun compile-angel--hook-after-load-functions (file)
  "Compile FILE after load."
  (let ((file (compile-angel--find-el-file file)))
    (when file
      (if (not (compile-angel--is-el-file file))
          (compile-angel--debug-message
           "compile-angel--hook-after-load-functions: IGNORE: %s" file)
        (compile-angel--debug-message
         "compile-angel--hook-after-load-functions: COMPILE: %s" file)
        (let ((compile-angel--force-compilation t))
          (compile-angel--entry-point file))))))

(defun compile-angel--update-el-file-regexp (symbol new-value
                                                    _operation _where)
  "Update the `compile-angel--el-file-regexp' variable.
SYMBOL is the symbol.
NEW-VALUE is the value of the variable."
  (when (eq symbol 'load-file-rep-suffixes)
    (compile-angel--debug-message
     "WATCHER: Update compile-angel--el-file-regexp: %s" new-value)
    (setq compile-angel--el-file-extensions
          (mapcar (lambda (ext) (concat ".el" ext)) load-file-rep-suffixes))
    (setq compile-angel--el-file-regexp
          (format "\\.el%s\\'" (regexp-opt new-value))))

  (when (eq symbol 'compile-angel-excluded-files)
    (compile-angel--debug-message
     "WATCHER: Update compile-angel-excluded-files: %s"
     compile-angel-excluded-files)
    (let ((path-suffixes-regexp nil))
      ;; Process `compile-angel-excluded-files' to generate regular
      ;; expressions.
      ;; For each suffix:
      ;; - If it ends with `.el`, remove the `.el` and concatenate it with
      ;;   `compile-angel--el-file-regexp`, then add \\' at the end.
      ;; - Otherwise, convert it into a regular expression and add \\' at the
      ;;   end.
      (dolist (suffix new-value)
        (when (and suffix (not (string= suffix "")))
          (let* ((el-suffix-p (string-suffix-p ".el" suffix))
                 (suffix-without-el (if el-suffix-p
                                        (string-remove-suffix ".el" suffix)
                                      suffix))
                 (el-file-regexp (if (and el-suffix-p
                                          compile-angel--el-file-regexp)
                                     compile-angel--el-file-regexp)))
            (push (concat (regexp-quote suffix-without-el)
                          el-file-regexp
                          (unless (string-prefix-p "\\'" el-file-regexp)
                            "\\'" ))
                  path-suffixes-regexp))))
      (setq compile-angel--excluded-path-suffixes-regexps
            (nreverse path-suffixes-regexp)))))

(defvar compile-angel--init-completed nil)

(defun compile-angel--init ()
  "Initialize internal variables."
  (unless compile-angel--init-completed
    ;; load-file-rep-suffixes
    (compile-angel--update-el-file-regexp 'load-file-rep-suffixes
                                          load-file-rep-suffixes
                                          nil nil)
    (add-variable-watcher 'load-file-rep-suffixes
                          #'compile-angel--update-el-file-regexp)

    ;; compile-angel--update-el-file-regexp
    (compile-angel--update-el-file-regexp 'compile-angel-excluded-files
                                          compile-angel-excluded-files
                                          nil nil)
    (add-variable-watcher 'compile-angel-excluded-files
                          #'compile-angel--update-el-file-regexp)
    (setq compile-angel--init-completed t)))

(defun compile-angel--is-el-file (file)
  "Return non-nil if FILE is an el-file."
  (when compile-angel--el-file-regexp
    ;; A variable watcher (the `compile-angel--update-el-file-regexp` function)
    ;; dynamically updates `compile-angel--el-file-regexp` whenever
    ;; `load-file-rep-suffixes` is modified.
    (string-match-p compile-angel--el-file-regexp file)))

;;;###autoload
(define-minor-mode compile-angel-on-load-mode
  "Toggle `compile-angel-mode' then compiles .el files before they are loaded."
  :global t
  :lighter " CAngelL"
  :group 'compile-angel
  (if compile-angel-on-load-mode
      (progn
        ;; Init
        (compile-angel--init)
        ;; Compile load-history
        (when compile-angel-on-load-compile-load-history
          (compile-angel--compile-load-history))
        ;; After load hook
        (when compile-angel-on-load-hook-after-load-functions
          (add-hook 'after-load-functions #'compile-angel--hook-after-load-functions))
        ;; Compile features
        (when compile-angel-on-load-compile-features
          (compile-angel--compile-features))
        ;; Advices
        (when compile-angel-on-load-advise-require
          (advice-add 'require :before #'compile-angel--advice-before-require))
        (when compile-angel-on-load-advise-load
          (advice-add 'load :before #'compile-angel--advice-before-load)))
    ;; Hooks
    (remove-hook 'after-load-functions #'compile-angel--hook-after-load-functions)
    ;; Advices
    (advice-remove 'require #'compile-angel--advice-before-require)
    (advice-remove 'load #'compile-angel--advice-before-load)))

;;;###autoload
(define-minor-mode compile-angel-on-save-mode
  "Toggle `compile-angel-mode'that compiles .el file when saved."
  :global t
  :lighter " CAngelSg"
  :group 'compile-angel
  (if compile-angel-on-save-mode
      (progn
        (compile-angel--init)
        (add-hook 'after-save-hook #'compile-angel--compile-on-save 99))
    (remove-hook 'after-save-hook #'compile-angel--compile-on-save)))

;;;###autoload
(define-minor-mode compile-angel-on-save-local-mode
  "Toggle `compile-angel-mode'that compiles .el file when saved."
  :global nil
  :lighter " CAngelSl"
  :group 'compile-angel
  (if compile-angel-on-save-local-mode
      (progn
        (compile-angel--init)
        (add-hook 'after-save-hook #'compile-angel--compile-on-save 99 t))
    (remove-hook 'after-save-hook #'compile-angel--compile-on-save t)))

(provide 'compile-angel)
;;; compile-angel.el ends here
