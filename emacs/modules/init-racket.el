;;; init-racket.el --- Racket Mode Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures `racket-mode` for Racket language support in Emacs.

;;; Code:

;; --- 1. `racket-mode` (Racket Language Support) ---
(use-package racket-mode
  :straight t
  :mode "\\.rkt\\'"
  :hook
  (racket-mode . (lambda ()
                   ;; Example: Bind F5 to run the Racket program.
                   ;; (define-key racket-mode-map (kbd "<f5>") 'racket-run)
                   ;; Example: Enable `electric-pair-mode` for auto-pairing parentheses.
                   ;; (electric-pair-mode)
                   ))
  :config
  ;; Additional `racket-mode` configurations can go here.
  ;; For instance, if you want to set a specific Racket executable path:
  ;; (setq racket-program "/usr/local/bin/racket")
  )

(provide 'init-racket)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-racket.el ends here
