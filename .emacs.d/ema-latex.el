;;; ema-latex.el -- LaTeX setup
;;
;; Author: Mathias Dannesbo <neic@neic.dk>
;; Time-stamp: <2014-03-22 15:55:08 (neic)>
;;
;; Inspired by prelude-latex.el
;; (http://www.emacswiki.org/cgi-bin/wiki/Prelude)

;; AUCTeX config
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq-default TeX-master nil)

;; generate pdfs
(setq TeX-PDF-mode t)

;; autosave before compiling
(setq TeX-save-query nil)

;; make the OS decide what viewer to use
(setq TeX-view-program-list '(("open" "open %s.pdf")))
(setq TeX-view-program-selection '((output-pdf "open")))

;; use fold-mode
(add-hook 'LaTeX-mode-hook (lambda ()
                             (TeX-fold-mode 1)))

;; fold every time a new file is found
(add-hook 'LaTeX-mode-hook 'TeX-fold-buffer t)

;; calc for single line LaTeX math
(setq calc-embedded-open-formula "^")
(setq calc-embedded-close-formula "\\\\?\\\\?\n")

(provide 'ema-latex)
