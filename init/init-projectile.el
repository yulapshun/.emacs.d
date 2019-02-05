(projectile-global-mode 1)

(setq projectile-globally-ignored-directories
      (cons "node_modules" projectile-globally-ignored-directories))

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(provide 'init-projectile)
