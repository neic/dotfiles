(defvar ema-dir "~/.emacs.d/")

(add-to-list 'load-path ema-dir)

;; config changes made through the customize UI will be store here
(setq custom-file "~/.emacs.d/custom.el")

(require 'ema-packages)
(require 'ema-editor)
(require 'ema-ui)
(require 'ema-latex)
(require 'ema-global-keybindings)

