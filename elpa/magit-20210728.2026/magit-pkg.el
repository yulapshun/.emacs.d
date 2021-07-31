(define-package "magit" "20210728.2026" "A Git porcelain inside Emacs."
  '((emacs "25.1")
    (dash "20210330")
    (git-commit "20210701")
    (magit-section "20210701")
    (transient "20210701")
    (with-editor "20210524"))
  :commit "ad406cd28b4e08ddb6964c168c7c8dffb5debbc6" :authors
  '(("Marius Vollmer" . "marius.vollmer@gmail.com")
    ("Jonas Bernoulli" . "jonas@bernoul.li"))
  :maintainer
  '("Jonas Bernoulli" . "jonas@bernoul.li")
  :keywords
  '("git" "tools" "vc")
  :url "https://github.com/magit/magit")
;; Local Variables:
;; no-byte-compile: t
;; End:
