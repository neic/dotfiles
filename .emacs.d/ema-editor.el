;;; ema-editor.el -- Core editor enhancement.
;;
;; Author: Mathias Dannesbo <neic@neic.dk>
;; Time-stamp: <2014-03-22 14:59:08 (neic)>
;;
;; Inspired by prelude-editor.el
;; (http://www.emacswiki.org/cgi-bin/wiki/Prelude)

(setq user-mail-address "neic@neic.dk")

;; Death to the tabs!
(setq-default indent-tabs-mode nil)

;; delete the selection with a keypress
(delete-selection-mode t)

;; highlight when searching and replacing
(setq search-highlight t
      query-replace-highlight t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; smart pairing
(electric-pair-mode t)

;; show-paren-mode: subtle highlighting of matching parens
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; savehist: save some history
(setq savehist-additional-variables    ;; also save...
      '(search ring regexp-search-ring)    ;; ... my search entries
      savehist-autosave-interval 60        ;; save every minute (default: 5 min)
      savehist-file (concat "~/.emacs.d" "/savehist"))   ;; keep my home clean
(savehist-mode t)                      ;; do customization before activation

;; time-stamps
;; when there's "Time-stamp: <>" in the first 10 lines of the file
(setq time-stamp-active t
      ;; check first 10 buffer lines for Time-stamp: <>
      time-stamp-line-limit 10
      time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%u)") ; date format
(add-hook 'write-file-hooks 'time-stamp) ; update when saving

;; ido-mode
(ido-mode t)
 (setq ido-enable-preix t
       ido-create-new-buffer 'always
       ido-max-prospects 10
       ido-default-file-method 'selected-window)

;; auto-completion in minibuffer
(icomplete-mode +1)
(set-default 'imenu-auto-rescan t)

;; flyspell-mode
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

(let ((langs '("english" "dansk")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))

(defun ema-cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(defun ema-turn-on-flyspell ()
   "Force flyspell-mode on using a positive argument.  For use in hooks."
   (interactive)
   (flyspell-mode 1))

(add-hook 'message-mode-hook 'ema-turn-on-flyspell)
(add-hook 'text-mode-hook 'ema-turn-on-flyspell)

;; Auto complete
(require 'auto-complete-config)
(global-auto-complete-mode t)

;; Yasnippet
(require 'yasnippet)
(yas-load-directory (concat ema-dir "snippets/"))
(yas-global-mode 1)

;; ajc-java-complete
(add-to-list 'load-path (concat ema-dir "plugins/ajc/"))
(require 'ajc-java-complete-config)
(add-hook 'java-mode-hook 'ajc-java-complete-mode)
(add-hook 'find-file-hook 'ajc-4-jsp-find-file-hook)

;; tramp, for sudo access
(require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")

;; edit as sudo
(defun ema-sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Petite Chez Scheme
(setq scheme-program-name "petite")

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(provide 'ema-editor)
