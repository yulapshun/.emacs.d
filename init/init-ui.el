(add-hook 'after-init-hook
          (lambda()
            (if (display-graphic-p)
                (load-theme 'solarized-dark)
              (load-theme 'tsdh-dark))
            (set-face-attribute 'whitespace-tab nil :background "DeepSkyBlue")))

(provide 'init-ui)