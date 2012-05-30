;;; ema-keybindings.el -- Core editor enhancement.
;;
;; Author: Mathias Dannesbo <neic@neic.dk>
;; Inspired by prelude-editor.el
;; (http://www.emacswiki.org/cgi-bin/wiki/Prelude)

;; Use the hyper key
(setq x-hyper-keysym 'hyper)

;; Open as root
(global-set-key (kbd "C-x C-a") 'ema-sudo-edit)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; cycle through buffers
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; toggle ispell language
(global-set-key (kbd "<f6>") 'ema-cycle-ispell-languages)

;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") 'menu-bar-mode)

;; real Emacs hackers don't use the arrow keys
(global-set-key (kbd "<up>") (lambda ()
                               (interactive)
                               (message "Arrow key navigation is disabled. Use C-p instead.")))
(global-set-key (kbd "<down>") (lambda ()
                                 (interactive)
                                 (message "Arrow key navigation is disabled. Use C-n instead.")))
(global-set-key (kbd "<left>") (lambda ()
                                 (interactive)
                                 (message "Arrow key navigation is disabled. Use C-b instead.")))
(global-set-key (kbd "<right>") (lambda ()
                                  (interactive)
                                  (message "Arrow key navigation is disabled. Use C-f instead.")))

(global-set-key (kbd "A-a") "α") ;; alpha
(global-set-key (kbd "A-b") "β") ;; beta
(global-set-key (kbd "A-c") "ξ") ;; xi
(global-set-key (kbd "A-d") "δ") ;; delta
(global-set-key (kbd "A-e") "ε") ;; epsilon
(global-set-key (kbd "A-f") "φ") ;; phi
(global-set-key (kbd "A-g") "γ") ;; gamma
(global-set-key (kbd "A-h") "η") ;; eta
(global-set-key (kbd "A-i") "ι") ;; iota
;;(global-set-key (kbd "A-j") "")
(global-set-key (kbd "A-k") "κ") ;; kappa
(global-set-key (kbd "A-l") "λ") ;; lambda
(global-set-key (kbd "A-m") "μ") ;; mu
(global-set-key (kbd "A-n") "ν") ;; nu
(global-set-key (kbd "A-o") "ο") ;; omnicron
(global-set-key (kbd "A-p") "π") ;; pi
(global-set-key (kbd "A-q") "θ") ;; theta
(global-set-key (kbd "A-r") "ρ") ;; roh
(global-set-key (kbd "A-s") "ς") ;; sigma
(global-set-key (kbd "A-t") "τ") ;; tau
(global-set-key (kbd "A-u") "υ") ;; upsilon
(global-set-key (kbd "A-v") "σ") ;; sigma
(global-set-key (kbd "A-w") "ω") ;; omega
(global-set-key (kbd "A-x") "χ") ;; chi
(global-set-key (kbd "A-y") "ψ") ;; psi
(global-set-key (kbd "A-z") "ζ") ;; zeta

(global-set-key (kbd "A-A") "Α") ;; Alpha
(global-set-key (kbd "A-B") "Β") ;; Beta
(global-set-key (kbd "A-C") "Ξ") ;; Xi
(global-set-key (kbd "A-D") "Δ") ;; Delta
(global-set-key (kbd "A-E") "Ε") ;; Epsilon
(global-set-key (kbd "A-F") "Φ") ;; Phi
(global-set-key (kbd "A-G") "Γ") ;; Gamma
(global-set-key (kbd "A-H") "Η") ;; Eta
(global-set-key (kbd "A-I") "Ι") ;; Iota
;;(global-set-key (kbd "A-J") "")
(global-set-key (kbd "A-K") "Κ") ;; Kappa
(global-set-key (kbd "A-L") "Λ") ;; Lambda
(global-set-key (kbd "A-M") "Μ") ;; Mu
(global-set-key (kbd "A-N") "Ν") ;; Nu
(global-set-key (kbd "A-O") "Ο") ;; Omnicron
(global-set-key (kbd "A-P") "Π") ;; Pi
(global-set-key (kbd "A-Q") "Θ") ;; Theta
(global-set-key (kbd "A-R") "Ρ") ;; Rho
(global-set-key (kbd "A-S") "Σ") ;; Sigma
(global-set-key (kbd "A-T") "Τ") ;; Tau
(global-set-key (kbd "A-U") "Υ") ;; Upsilon
(global-set-key (kbd "A-V") "") 
(global-set-key (kbd "A-W") "Ω") ;; Omega
(global-set-key (kbd "A-X") "Χ") ;; Chi
(global-set-key (kbd "A-Y") "Ψ") ;; Psi
(global-set-key (kbd "A-Z") "Ζ")



(global-set-key (kbd "H-½") "")
(global-set-key (kbd "H-1") "")
(global-set-key (kbd "H-2") "")
(global-set-key (kbd "H-3") "")
(global-set-key (kbd "H-4") "")
(global-set-key (kbd "H-5") "")
(global-set-key (kbd "H-6") "")
(global-set-key (kbd "H-7") "")
(global-set-key (kbd "H-8") "⟨") ;; Angle bracket left
(global-set-key (kbd "H-9") "⟩") ;; Angle bracket right
(global-set-key (kbd "H-0") "≈")
(global-set-key (kbd "H-+") "")

(global-set-key (kbd "H-q") "ℚ") ;; Blackboard Q
(global-set-key (kbd "H-w") "∆")
(global-set-key (kbd "H-e") "∃") ;; Logic: there exists [\exists]
(global-set-key (kbd "H-r") "ℝ") ;; Blackboard R
(global-set-key (kbd "H-t") "⊤") ;; True
(global-set-key (kbd "H-y") "⊂") ;; Set theory: subset
(global-set-key (kbd "H-u") "∪") ;; Set theory: union
(global-set-key (kbd "H-i") "∩") ;; Set theory: intersection
(global-set-key (kbd "H-o") "⊃") ;; Set theory: superset
(global-set-key (kbd "H-p") "ℙ") ;; Blackboard P
(global-set-key (kbd "H-å") "∖") ;; Set theory: complement
(global-set-key (kbd "H-a") "∀") ;; Logic: for all [\forall]
(global-set-key (kbd "H-s") "")
(global-set-key (kbd "H-d") "")
(global-set-key (kbd "H-f") "⊥") ;; False
(global-set-key (kbd "H-g") "")
(global-set-key (kbd "H-h") "ℍ") ;; Blackboard H
(global-set-key (kbd "H-j") "")
(global-set-key (kbd "H-k") "")
(global-set-key (kbd "H-l") "")
(global-set-key (kbd "H-æ") "")
(global-set-key (kbd "H-ø") "")
(global-set-key (kbd "H-z") "ℤ") ;; Blackboard 
(global-set-key (kbd "H-x") (kbd "C-x 8 x")) ;; Multiplication ×
(global-set-key (kbd "H-c") "ℂ") ;; Blackbord C
(global-set-key (kbd "H-v") "")
(global-set-key (kbd "H-b") "")
(global-set-key (kbd "H-n") "ℕ") ;; Blackboard N
(global-set-key (kbd "H-m") "")
(global-set-key (kbd "H-,") "")
(global-set-key (kbd "H-.") "…") ;; Ellipsis [\dots]
(global-set-key (kbd "H--") "")

(global-set-key (kbd "H-Q") "")
(global-set-key (kbd "H-W") "")
(global-set-key (kbd "H-E") "∅") ;; Set theory: empty set [\emptyset \varnothing]
(global-set-key (kbd "H-R") "")
(global-set-key (kbd "H-T") "")
(global-set-key (kbd "H-Y") "")
(global-set-key (kbd "H-U") "⊆") ;; Set theory: subset
(global-set-key (kbd "H-I") "⊇") ;; Set theory: superset
(global-set-key (kbd "H-O") "∈") ;; Set theory: membership
(global-set-key (kbd "H-P") "")
(global-set-key (kbd "H-Å") "")
(global-set-key (kbd "H-A") "")
(global-set-key (kbd "H-S") "")
(global-set-key (kbd "H-D") "")
(global-set-key (kbd "H-F") "")
(global-set-key (kbd "H-G") "")
(global-set-key (kbd "H-H") "")
(global-set-key (kbd "H-J") "")
(global-set-key (kbd "H-K") "")
(global-set-key (kbd "H-L") "")
(global-set-key (kbd "H-Æ") "")
(global-set-key (kbd "H-Ø") "")
(global-set-key (kbd "H-Z") "")
(global-set-key (kbd "H-X") "")
(global-set-key (kbd "H-C") "")
(global-set-key (kbd "H-V") "")
(global-set-key (kbd "H-B") "")
(global-set-key (kbd "H-N") "")
(global-set-key (kbd "H-M") "")
(global-set-key (kbd "H-;") "")
(global-set-key (kbd "H-:") "")
(global-set-key (kbd "H-_") "")


(provide 'ema-global-keybindings)
