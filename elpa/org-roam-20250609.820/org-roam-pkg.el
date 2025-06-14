;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org-roam" "20250609.820"
  "A database abstraction layer for Org-mode."
  '((emacs         "26.1")
    (dash          "2.13")
    (org           "9.6")
    (emacsql       "4.1.0")
    (magit-section "3.0.0"))
  :url "https://github.com/org-roam/org-roam"
  :commit "c17295134510c8759baad0e8a9f151029d5e5fe1"
  :revdesc "c17295134510"
  :keywords '("org-mode" "roam" "convenience")
  :authors '(("Jethro Kuan" . "jethrokuan95@gmail.com"))
  :maintainers '(("Jethro Kuan" . "jethrokuan95@gmail.com")))
