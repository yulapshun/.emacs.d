(add-hook 'after-init-hook
          (lambda()
            (if (display-graphic-p)
                (load-theme 'solarized-dark)
              (load-theme 'tsdh-dark))
1))

(provide 'init-ui)
