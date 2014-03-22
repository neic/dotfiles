;;; ema-latex.el -- LaTeX setup
;;
;; Author: Mathias Dannesbo <neic@neic.dk>
;; Time-stamp: <2014-03-22 15:58:44 (neic)>
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

;; add -shell-escape engine variants
(setq TeX-engine-alist
      '((xetex_sh "XeTeX shell escape"
                  "xetex -file-line-error -shell-escape"
                  "xelatex -file-line-error -shell-escape"
                  "xetex")
        (luatex_sh "LuaTeX shell escape"
                   "luatex -file-line-error -shell-escape"
                   "lualatex -file-line-error -shell-escape"
                   "luatex")))


;; and make the corresponding local variable safe.
;; Warning: Make sure you know what code you are executing
(setq safe-local-variable-values '((TeX-engine . xelatex_sh)
                                   (TeX-engine . luatex_sh)))

;; use electric sub and superscript
(setq TeX-electric-sub-and-superscript t)

;; calc for single line LaTeX math
(setq calc-embedded-open-formula "^")
(setq calc-embedded-close-formula "\\\\?\\\\?\n")

(provide 'ema-latex)
