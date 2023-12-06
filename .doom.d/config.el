;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "BlexMono Nerd Font Mono" :size 13)
      doom-variable-pitch-font (font-spec :family "BlexMono Nerd Font" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(defun theme-variant ()
  (if (string= (plist-get (mac-application-state) :appearance) "NSAppearanceNameDarkAqua")
      'doom-vibrant
      'doom-one-light))

(setq doom-vibrant-brighter-comments t)
(setq doom-one-light-brighter-comments t)

(setq doom-theme (theme-variant))
(add-hook 'mac-effective-appearance-change-hook #'(lambda () (load-theme (theme-variant) 1)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; ---------------
;; Global settings
;; ---------------

; Overwrite modifier keys on macOS
(setq mac-command-modifier 'meta
      mac-option-modifier 'none)

; Disable everything other than git for version control to speedup TRAMP.
; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
; # How to speed up TRAMP?
(setq vc-handled-backends '(Git))
; Reuse the same ssh connection everywhere by inheriting ControlMaster from
; ~/.ssh/config for TRAMP.
; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
; # TRAMP does not use default ssh ControlPath
(setq tramp-use-ssh-controlmaster-options nil)

; Don't insert newlines in base64-encode-region when its given a
; prefix-argument.
; https://emacs.stackexchange.com/a/41225
(defun base64-encode-region-prefix-arg (&rest _args)
  "Pass prefix arg as third arg to `base64-encode-region'."
  (interactive "r\nP"))
(advice-add 'base64-encode-region :before #'base64-encode-region-prefix-arg)

;; Use lsp on large repos.
(after! lsp-mode
  (setq lsp-file-watch-threshold 3500)
  )

;; -----------
;; Major modes
;; -----------

(after! magit
  (setq magit-save-repository-buffers 'dontask)
  (add-hook 'git-commit-setup-hook 'end-of-line))

(after! vterm
  (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
  ;; Always use zsh on vterm remote machines.
  (add-to-list 'vterm-tramp-shells
               '("ssh" "zsh"))
  )

(after! org
  (setq org-startup-folded t)
  (setq org-modules '(org-id))
  (setq org-id-link-to-org-use-id t)
  (setq org-capture-templates
        '(
          ("t" "Simple TODO" entry
           (file #1="")
           "* TODO %^{Title}\nCaptured On: %U\n%?"
           )
          ("T" "Timed TODO" entry
           (file #1="")
           "* TODO %^{Title}\nSCHEDULED: %^{Schedule}t DEADLINE: %^{Deadline}t\nCaptured On: %U\n%?"
           )
          ;; For/from https://github.com/sprig/org-capture-extension
          ("p" "Protocol" entry (file "")
           "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
          ("L" "Protocol Link" entry (file "")
           "* %? [[%:link][%:description]] \nCaptured On: %U")
          ))
  ;; From https://github.com/doomemacs/doomemacs/blob/master/modules/lang/org/config.el
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s!)"  ; A task that is in progress
           "WAIT(w!)"  ; Something external is holding up this task
           "HOLD(h!)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d!)"  ; Task successfully completed
           "KILL(k!)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)")
          ))
  ;; Search recursively for org-agenda files. https://stackoverflow.com/a/41969519
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
  )


(use-package! gptai)
;;(setq gptai-api-key "")
(setq gptai-username "md@magenta-aps.dk")
;;(setq gptai-org "org-oABnHRQlPUh176CjMBtt11Ao")
(setq gptai-model "gpt-4")

(after! gptai
  (defun ai-replace (prompt)
    (let ((gptai-prompt (concat prompt
                            (buffer-substring-no-properties
                             (region-beginning)
                             (region-end))
                          )))
    (let ((response (gptai-turbo-request gptai-prompt)))
      (delete-region (region-beginning)
                     (region-end))
      (insert response))))

  (defun ai-shorten-commit-message ()
    "Shorten a commit message in region to less than 50 characters."
    (interactive)
    (ai-replace "Shorten this git commit message to be shorter than 50 characters.\n")
    )
  (global-set-key (kbd "C-c g s") 'ai-shorten-commit-message)

  (defun ai-fix-merge-conflict ()
    (interactive)
    (let ((gptai-prompt (concat "How do I fix the following git merge conflict?\n"
                        (buffer-substring-no-properties
                             (region-beginning)
                             (region-end))
                          )))
    (gptai-turbo-query gptai-prompt)
    ))
  (global-set-key (kbd "C-c g c") 'ai-fix-merge-conflict)

)

(use-package! magit-lfs)
