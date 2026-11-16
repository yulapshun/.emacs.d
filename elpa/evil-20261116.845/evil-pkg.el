;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "evil" "20261116.845"
  "Extensible vi layer."
  '((emacs    "24.1")
    (cl-lib   "0.5")
    (goto-chg "1.6")
    (nadvice  "0.3"))
  :url "https://github.com/emacs-evil/evil"
  :commit "76e094309f615594ced89afa7b32c2051715b402"
  :revdesc "76e094309f61"
  :keywords '("emulations")
  :maintainers '(("Tom Dalziel" . "tom.dalziel@gmail.com")))
