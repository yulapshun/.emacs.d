;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20260711.1257"
  "A Git porcelain inside Emacs."
  '((emacs         "28.1")
    (compat        "31.0")
    (cond-let      "1.1")
    (llama         "1.0")
    (magit-section "4.6")
    (seq           "2.24")
    (transient     "0.13")
    (with-editor   "3.5"))
  :url "https://github.com/magit/magit"
  :commit "fe56c0d10a91d00aed65f831b8435a7224bc7730"
  :revdesc "fe56c0d10a91"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
