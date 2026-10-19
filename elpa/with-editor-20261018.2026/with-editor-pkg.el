;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "with-editor" "20261018.2026"
  "Use the Emacsclient as $EDITOR."
  '((emacs    "28.1")
    (compat   "31.0")
    (cond-let "1.1")
    (llama    "1.0"))
  :url "https://github.com/magit/with-editor"
  :commit "fd559b37da8eb7afd93bf48db266410b1821dc02"
  :revdesc "fd559b37da8e"
  :keywords '("processes" "terminals")
  :authors '(("Jonas Bernoulli" . "emacs.with-editor@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.with-editor@jonas.bernoulli.dev")))
