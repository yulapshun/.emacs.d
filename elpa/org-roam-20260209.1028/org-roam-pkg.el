;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "org-roam" "20260209.1028"
  "A database abstraction layer for Org-mode."
  '((emacs         "26.1")
    (compat        "30.1")
    (org           "9.6")
    (emacsql       "4.1.0")
    (magit-section "3.0.0"))
  :url "https://github.com/org-roam/org-roam"
  :commit "b4857fd7a140361883dfb95e1193ee42698a4afb"
  :revdesc "b4857fd7a140"
  :keywords '("org-mode" "roam" "convenience")
  :authors '(("Jethro Kuan" . "jethrokuan95@gmail.com"))
  :maintainers '(("Jethro Kuan" . "jethrokuan95@gmail.com")))
