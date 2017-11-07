(require 'magit)

(magit-define-popup-switch 'magit-commit-popup ?E
  "Allow empty message" "--allow-empty-message")

(provide 'init-magit)
