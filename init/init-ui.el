(defun init-theme (frame)
  (mapcar #'disable-theme custom-enabled-themes)
  (if (display-graphic-p frame)
      (load-theme 'solarized-dark)
    (load-theme 'tsdh-dark))
  (set-face-attribute 'whitespace-tab nil :background "SkyBlue4"))

(add-hook 'after-init-hook
          (lambda ()
            (mapc 'init-theme (frame-list))))
;; Handle start from daemon
(add-hook 'after-make-frame-functions 'init-theme)

(tool-bar-mode -1)

(provide 'init-ui)
