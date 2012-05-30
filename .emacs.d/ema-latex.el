;;; ema-latex.el -- LaTeX setup
;;
;; Author: Mathias Dannesbo <neic@neic.dk>
;; Time-stamp: <2012-05-29 12:07:15 (neic)>
;;
;; Inspired by prelude-latex.el
;; (http://www.emacswiki.org/cgi-bin/wiki/Prelude)

;; AUCTeX config
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq-default TeX-master nil)

;; use pdflatex
(setq TeX-PDF-mode t)

;;autosave before compiling
(setq TeX-save-query nil)

;; use evince as pdf viewer
(setq TeX-view-program-selection '((output-pdf "evince")))

;; use fold-mode
(add-hook 'LaTeX-mode-hook (lambda ()
                             (TeX-fold-mode 1)))

;; fold every time a new file is found
(add-hook 'LaTeX-mode-hook 'TeX-fold-buffer t)


;; add XeLaTeX to the command list
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list
     		'("XeLaTeX" "xelatex -interaction=nonstopmode %s"
		  TeX-run-command t t :help "Run xelatex") t))

;; calc for single line LaTeX math
(setq calc-embedded-open-formula "^")
(setq calc-embedded-close-formula "\\\\?\\\\?\n")

(provide 'ema-latex)
