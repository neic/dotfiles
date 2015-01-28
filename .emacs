(defvar ema-dir "~/.emacs.d/")

(add-to-list 'load-path (concat ema-dir "ema/"))

;; config changes made through the customize UI will be store here
(setq custom-file "~/.emacs.d/custom.el")

(require 'ema-packages)
(require 'ema-osx)
(require 'ema-editor)
(require 'ema-ui)
(require 'ema-latex)
(require 'ema-global-keybindings)

