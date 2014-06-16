;;; ema-ui.el -- UI optimizations and tweaks.
;;
;; Author: Mathias Dannesbo <neic@neic.dk>
;; Time-stamp: <2014-06-16 16:01:34 (neic)>
;;
;; Inspired by prelude-ui.el
;; (http://www.emacswiki.org/cgi-bin/wiki/Prelude)

;; the toolbar is just a waste of valuable screen estate
(tool-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; bigger face
(set-face-attribute 'default nil :height 140)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; custom Emacs 24 color themes support
(add-to-list 'custom-theme-load-path (concat ema-dir "themes/"))
(load-theme 'zenburn t)

(provide 'ema-ui)
