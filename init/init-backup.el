(setq backup-directory (concat user-emacs-directory "backup/"))

(setq backup-directory-alist
      `((".*" . ,backup-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,backup-directory t)))

(message "Deleting old backup files...")
(let ((month (* 60 60 24 30))
      (current (float-time (current-time))))
  (dolist (file (directory-files backup-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (cl-fifth (file-attributes file))))
                  month))
      (message "%s" file)
      (delete-file file))))

(provide 'init-backup)
