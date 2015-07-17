;;; ema-gnus.el -- gnus setup
;;
;; Author: Mathias Dannesbo <neic@neic.dk>
;; Time-stamp: <2015-07-02 14:58:46 (neic)>
;;

(setq gnus-select-method 
      '(nnmaildir "GMail" 
                  (directory "~/Mail/Gmail/")
                  (directory-files nnheader-directory-files-safe) 
                  (get-new-mail nil)))

;; (define-key gnus-group-mode-map (kbd "vo")
;;   '(lambda ()
;;      (interactive)
;;      (shell-command "offlineimap&" "*offlineimap*" nil)))

(provide 'ema-gnus)
