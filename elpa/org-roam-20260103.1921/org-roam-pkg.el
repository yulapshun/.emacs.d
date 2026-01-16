;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org-roam" "20260103.1921"
  "A database abstraction layer for Org-mode."
  '((emacs         "26.1")
    (compat        "30.1")
    (dash          "2.13")
    (org           "9.6")
    (emacsql       "4.1.0")
    (magit-section "3.0.0"))
  :url "https://github.com/org-roam/org-roam"
  :commit "b95d04cbd223e369b9578b0dc47bbdd376d40dc3"
  :revdesc "b95d04cbd223"
  :keywords '("org-mode" "roam" "convenience")
  :authors '(("Jethro Kuan" . "jethrokuan95@gmail.com"))
  :maintainers '(("Jethro Kuan" . "jethrokuan95@gmail.com")))
