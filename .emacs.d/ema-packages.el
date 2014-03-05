;;; ema-packages.el -- ELPA packages
;;
;; Author: Mathias Dannesbo <neic@neic.dk>
;; Time-stamp: <2014-02-26 13:37:53 (neic)>
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
  '(auctex auto-complete solarized-theme yasnippet zenburn-theme exec-path-from-shell git-gutter)
  "A list of packages to ensure are installed at launch.")

(dolist (p prelude-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'ema-packages)
