;;; init-zone.el --- Zone Mode Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file initializes and customizes `zone-mode`, which displays a chosen
;; program when Emacs is idle.

;;; Code:

;; --- 1. `zone` (Idle Display Mode) ---
(use-package zone
  :straight nil
  :defer 5
  :config
  (zone-mode)

  (zone-when-idle 300) ; Corrected: Call as a function, not set with setq.

  ;; Define a custom function to choose and run a specific zone program.
  ;; This makes it easy to interactively select which zone program to display.
  (defun zone-choose (pgm)
    "Choose a PGM to run for `zone'.
PGM is the symbol name of the zone program (e.g., `zone-blinky', `zone-bubbles')."
    (interactive
     ;; Use `completing-read` to offer available zone programs as choices.
     (list (intern (completing-read "Program: " (mapcar 'symbol-name zone-programs)))))
    ;; Temporarily set `zone-programs` to just the chosen program, then run `zone`.
    (let ((zone-programs (list (intern pgm))))
      (zone)))

  ;; Optional: Add keybindings for `zone-mode` commands.
  ;; For example, to quickly enable/disable or choose a program.
  ;; (define-key global-map (kbd "C-c z") 'zone-choose) ; Bind C-c z to choose a zone program
  ;; (define-key global-map (kbd "C-c Z") 'zone-mode)    ; Bind C-c Z to toggle zone-mode
  )

(provide 'init-zone)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-zone.el ends here
