;;; ema-editor.el -- Core editor enhancement.
;;
;; Author: Mathias Dannesbo <neic@neic.dk>
;; Time-stamp: <2015-07-17 22:45:25 (neic)>
;;
;; Inspired by prelude-editor.el
;; (http://www.emacswiki.org/cgi-bin/wiki/Prelude)

(setq user-mail-address "neic@neic.dk")

;; White space control
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)

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

;; ibuffer groups
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("erc"   (mode . erc-mode))
               ("gnus" (or
                        (mode . message-mode)
                        (mode . bbdb-mode)
                        (mode . mail-mode)
                        (mode . gnus-group-mode)
                        (mode . gnus-summary-mode)
                        (mode . gnus-article-mode)
                        (name . "^\\.bbdb$")
                        (name . "^\\.newsrc-dribble")))
               ("java"  (mode . java-mode))
               ("latex" (or
                         (mode . tex-mode)
                         (mode . latex-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")
                         (name . "^\\*Completions\\*$")))
               ))))

;; flyspell-mode
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

(let ((langs '("en" "da")))
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
(add-hook 'after-init-hook 'global-company-mode)
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay 0)                          ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

;; Yasnippet
(require 'yasnippet)
(yas-load-directory (concat ema-dir "snippets/"))
(yas-global-mode 1)

(with-eval-after-load 'company
(push '(company-semantic :with company-yasnippet) company-backends))

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

;; edit as sudo over ssh
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;; git gutter everywhere
(global-git-gutter-mode +1)

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (if (not indent-tabs-mode)
      (untabify (point-min) (point-max))))

(defun comment-line (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above."
  (interactive "p")
  (comment-or-uncomment-region
   (line-beginning-position)
   (goto-char (line-end-position n)))
  (forward-line 1)
  (back-to-indentation))

;; -----------
;; Major modes
;; -----------

;; python-mode
(add-hook 'python-mode-hook 'anaconda-mode 'turn-on-eldoc-mode)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-anaconda))

;; markdown-mode
(add-to-list 'load-path (concat ema-dir "plugins/markdown-mode/"))
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; go-mode
(require 'company-go)
(add-hook 'go-mode-hook
          (lambda ()
            (setq-default)
            (setq tab-width 4)
            (setq indent-tabs-mode t)))

;; cc-mode
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)

;; c-mode
(add-hook 'c-mode-common-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "C-c C-c") 'compile)
            (define-key c-mode-base-map (kbd "C-c ;") 'comment-region)))


;; scheme-mode
(setq scheme-program-name "petite")

;; erc-mode
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; dired-mode
(setq openwith-associations '(("\\.pdf\\'" "open" (file))))
(openwith-mode t)

;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)


(provide 'ema-editor)
