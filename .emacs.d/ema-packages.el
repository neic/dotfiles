;;; ema-packages.el -- ELPA packages
;;
;; Author: Mathias Dannesbo <neic@neic.dk>
;; Time-stamp: <2012-03-24 01:58:24 (neic)>
;;
;; Inspired by prelude-packages.el
;; (http://www.emacswiki.org/cgi-bin/wiki/Prelude)

(require 'package)
(setq package-archives
             '(("marmalade" . "http://marmalade-repo.org/packages/")
	     ("ELPA" . "http://tromey.com/elpa/") 
	     ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar prelude-packages
  '(auctex yasnippet)
  "A list of packages to ensure are installed at launch.")

(dolist (p prelude-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'ema-packages)
