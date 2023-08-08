(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d" "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" "969a67341a68becdccc9101dc87f5071b2767b75c0b199e0ded35bd8359ecd69" "521682d356435276b4bfb60cd134681aeaf4f2e4ee625456c04285da725ebf7c" default))
 '(delete-selection-mode nil)
 '(package-selected-packages
   '(gcmh smooth-scrolling good-scroll eglot popper markdown-mode sqlite3 treesit-auto org-roam-ui org-fragtog embark embark-consult orderless consult marginalia vertico org-roam org-roam-timestamps org nadvice json-mode avy dashboard undo-tree pyvenv pyvenv-auto ido-vertical-mode yasnippet-snippets company-box vundo yasnippet yaml-mode web-mode use-package symbol-overlay solarized-theme pug-mode projectile php-mode org-superstar magit ido-completing-read+ gruvbox-theme go-mode git-gutter flycheck flx-ido eyebrowse exec-path-from-shell dockerfile-mode company-web auto-compile all-the-icons-dired))
 '(safe-local-variable-values
   '((eval with-eval-after-load 'ox-latex
           (add-to-list 'org-latex-classes
                        '("tumarticle" "\\documentclass{tumarticle}"
                          ("\\section{%s}" . "\\section*{%s}")
                          ("\\subsection{%s}" . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
     (eval with-eval-after-load 'ox-latex
           (add-to-list 'org-latex-classes
                        '("tumbook" "\\documentclass{tumbook}"
                          ("\\chapter{%s}" . "\\chapter*{%s}")
                          ("\\section{%s}" . "\\section*{%s}")
                          ("\\subsection{%s}" . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
     (eval add-to-list 'org-latex-classes
           '("tumbook" "\\documentclass{tumbook}"
             ("\\chapter{%s}" . "\\chapter*{%s}")
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
     (org-export-with-section-numbers . t)
     (org-export-with-toc . t)
     (org-export-with-date . t)
     (org-export-with-author . t)
     (org-pretty-entities)))
 '(warning-suppress-types '((comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#1d2021" :foreground "#ebdbb2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(org-document-title ((t (:inherit default :weight bold :height 1.5 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :height 1.3))))
 '(org-level-2 ((t (:inherit default :weight bold :height 1.2))))
 '(org-level-3 ((t (:inherit default :weight bold :height 1.1))))
 '(org-level-4 ((t (:inherit default :weight bold))))
 '(org-level-5 ((t (:inherit default :weight bold))))
 '(org-level-6 ((t (:inherit default :weight bold))))
 '(org-level-7 ((t (:inherit default :weight bold))))
 '(org-level-8 ((t (:inherit default :weight bold)))))
