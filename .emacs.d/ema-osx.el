;;; ema-osx.el -- OSX specific changes
;;
;; Author: Mathias Dannesbo <neic@neic.dk>
;; Time-stamp: <2013-06-17 15:13:14 (neic)>
;;

(when (eq system-type 'darwin) 

  ;; Load PATH from shell 
  (exec-path-from-shell-initialize)

  ;; Bind option to ALT and command to META
  (setq mac-option-modifier 'alt)
  (setq mac-right-option-modifier nil)
  (setq mac-command-modifier 'meta)
  (setq mac-right-command-modifier 'hyper)

  ;; sets fn-delete to be right-delete
  (global-set-key [kp-delete] 'delete-char)

  ;; use mouse-3 for flyspell
  (eval-after-load "flyspell"
    '(progn
       (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
       (define-key flyspell-mouse-map [mouse-3] #'undefined)))

)

(provide 'ema-osx)
