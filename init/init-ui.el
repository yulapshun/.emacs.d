(defun init-theme (frame)
  (mapcar #'disable-theme custom-enabled-themes)
  (if (display-graphic-p frame)
      (progn
        (load-theme 'gruvbox-dark-medium)
        (set-face-background 'hl-line "#1d2021"))
    (load-theme 'tsdh-dark))
  (set-face-attribute 'whitespace-tab nil :background "SkyBlue4")
  (set-face-attribute 'whitespace-trailing nil :background "Coral3")
  ;; Disable symbol-overlay's temporary hightlight by setting it to background color
  ;; (set-face-attribute 'symbol-overlay-temp-face nil :background (face-attribute 'default :background))
  )

(add-hook 'after-init-hook
          (lambda ()
            (mapc 'init-theme (frame-list))))
;; Handle start from daemon
(add-hook 'after-make-frame-functions 'init-theme)

(tool-bar-mode -1)

(provide 'init-ui)
