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
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "BlexMono Nerd Font Mono" :size 13)
      doom-variable-pitch-font (font-spec :family "BlexMono Nerd Font" :size 13))
;; The Apple Color Emoji font give slightly to high glyphs causing uneven
;; lineheights. This is especially noticeable in the terminal when programs
;; redraws lines with emoji. We scale it down.
(add-to-list 'face-font-rescale-alist '("Apple Color Emoji" . 0.8))

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
(custom-set-faces!
  `(corfu-default :background ,(doom-color 'base2))
  `(corfu-current :background ,(doom-blend 'highlight 'bg 0.2))
  `(corfu-border :background ,(doom-color 'base6))
 )

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

;; Use lsp on large repos.
(after! lsp-mode
  (setq lsp-file-watch-threshold 3500)
  )

;; Set server socket elsewhere than $TMPDIR as it is not preserved in nix-shell.
;; https://github.com/NixOS/nix/pull/7492
(setq server-socket-dir "~/.emacs.d/")

(after! git-auto-commit-mode
  (setq gac-automatically-push-p t)
  (setq gac-automatically-add-new-files-p t)
  (setq gac-debounce-interval 5)
)

(use-package ultra-scroll
  :if (eq window-system 'mac)
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1))

;; -----
;; Tramp
;; -----

; Disable everything other than git for version control to speedup TRAMP.
;
; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
; # How to speed up TRAMP?
(setq vc-handled-backends '(Git))

; Reuse the same ssh connection everywhere by inheriting ControlMaster from
; ~/.ssh/config for TRAMP.
;
; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
; # TRAMP does not use default ssh ControlPath
(setq tramp-use-ssh-controlmaster-options nil)

; Disable file locks. Safe if no other Emacs sessions are modifying the same
; remote file.
;
; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
; # How to speed up TRAMP?
; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./#getting-started
(setq remote-file-name-inhibit-locks t)

; Even when chosing external methods (scp, rsync), files smaller than
; tramp-copy-size-limit, use inline methods. The default is 10kB, but
; experiments posted on coredumped.dev show that the cutoff is around 2MB.
;
; https://www.gnu.org/software/tramp/#External-methods-1
; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./#getting-started
(setq tramp-copy-size-limit (* 1024 1024)) ; 1MB

; Set the default method to rsync. The default method is used when accessing
; files with /-:hostname: . coredumped.dev says rsync is 3-4 times faster than
; the default scp after the initial transfer.
;
; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./#getting-started
(customize-set-variable 'tramp-default-method "rsync")


; Run remote commands directly instead of wrapping them in a shell. This greatly
; speeds up magit and git-gutter.
;
; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1
; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./#use-direct-async
(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))
(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)
(connection-local-set-profiles
 '(:application tramp :protocol "rsync")
 'remote-direct-async-process)
; A compatibility setting for magit is set below.


;; -----------
;; Major modes
;; -----------

(after! magit
  (setq magit-save-repository-buffers 'dontask)
  ; Get magit to work with the Tramp direct async setting above. This breaks DOS
  ; line endings.
  ;
  ; https://github.com/magit/magit/issues/5220
  (setq magit-tramp-pipe-stty-settings 'pty)
  (add-hook 'git-commit-setup-hook 'end-of-line)
  (transient-append-suffix 'magit-push "-u"
    '(1 "=s" "Skip gitlab pipeline" "--push-option=ci.skip"))
  (transient-append-suffix 'magit-push "=s"
    '(1 "=m" "Create MR" "--push-option=merge_request.create"))
  )
(use-package! magit-lfs)
(use-package! magit-prime)
(after! magit-prime
  (add-hook 'magit-pre-refresh-hook 'magit-prime-refresh-cache))

(use-package! eat)
(after! eat
  (map! :leader
        (:prefix-map ("o" . "open")
         :desc "Open eat at project root"            "t" #'eat-project
         ))
  (setq eat-query-before-killing-running-terminal 'auto
        eat-kill-buffer-on-exit 't )
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
  ;; Auto save all org mode buffers after inactivity
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)
  )

(after! apheleia
  (setf (alist-get 'terraform apheleia-formatters)
        '("tofu" "fmt" "-"))
  )

(use-package ellama
  :demand t
  :bind ("C-c g" . ellama-transient-main-menu)
  :init
  (setopt ellama-auto-scroll t)
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model "llama3.2:3b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-summarization-provider
          (make-llm-ollama
           :chat-model "llama3.2:3b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 32768))))
  (setopt ellama-coding-provider
          (make-llm-ollama
           :chat-model "qwen2.5-coder:14b-instruct-q6_K"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 32768))))
  (setopt ellama-naming-provider
          (make-llm-ollama
           :chat-model "llama3.2:3b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  (setopt ellama-translation-provider
          (make-llm-ollama
           :chat-model "llama3.2:3b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 32768)))))
