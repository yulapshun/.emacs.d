;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20250515.1348"
  "A Git porcelain inside Emacs."
  '((emacs         "27.1")
    (compat        "30.1.0.0")
    (llama         "0.6.2")
    (magit-section "4.3.5")
    (seq           "2.24")
    (transient     "0.8.8")
    (with-editor   "3.4.3"))
  :url "https://github.com/magit/magit"
  :commit "415bd700992a70e50fd6fdf8e37519bf22fa2aa7"
  :revdesc "415bd700992a"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
