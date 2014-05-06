;;; ema-packages.el -- ELPA packages
;;
;; Author: Mathias Dannesbo <neic@neic.dk>
;; Time-stamp: <2014-04-26 00:37:31 (neic)>
;;
;; Inspired by prelude-packages.el
;; (http://www.emacswiki.org/cgi-bin/wiki/Prelude)

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar prelude-packages
  '(auctex
    mediawiki
    go-mode
    yasnippet
    auto-complete
    auto-complete-auctex
    openwith
    git-gutter
    exec-path-from-shell
    zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p prelude-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'ema-packages)
