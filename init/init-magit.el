(require 'magit)

(magit-define-popup-switch 'magit-commit-popup ?E
  "Allow empty message" "--allow-empty-message")
(setq magit-completing-read-function 'magit-ido-completing-read)

(provide 'init-magit)
