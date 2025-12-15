;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org-roam" "20251125.729"
  "A database abstraction layer for Org-mode."
  '((emacs         "26.1")
    (compat        "30.1")
    (dash          "2.13")
    (org           "9.6")
    (emacsql       "4.1.0")
    (magit-section "3.0.0"))
  :url "https://github.com/org-roam/org-roam"
  :commit "f4ba41cf3d59084e182a5186d432afc9aa3fc423"
  :revdesc "f4ba41cf3d59"
  :keywords '("org-mode" "roam" "convenience")
  :authors '(("Jethro Kuan" . "jethrokuan95@gmail.com"))
  :maintainers '(("Jethro Kuan" . "jethrokuan95@gmail.com")))
