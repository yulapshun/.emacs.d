;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "evil" "20241214.59"
  "Extensible vi layer."
  '((emacs    "24.1")
    (cl-lib   "0.5")
    (goto-chg "1.6")
    (nadvice  "0.3"))
  :url "https://github.com/emacs-evil/evil"
  :commit "35fe630b3227bb44c54df0c4a265c3a5e151331a"
  :revdesc "35fe630b3227"
  :keywords '("emulations")
  :maintainers '(("Tom Dalziel" . "tom.dalziel@gmail.com")))
