;;; ema-ui.el -- UI optimizations and tweaks.
;;
;; Author: Mathias Dannesbo <neic@neic.dk>
;; Time-stamp: <2013-05-26 16:04:09 (neic)>
;;
;; Inspired by prelude-ui.el
;; (http://www.emacswiki.org/cgi-bin/wiki/Prelude)

;; the toolbar and menu is just a waste of valuable screen estate
(tool-bar-mode -1)
(menu-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

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
