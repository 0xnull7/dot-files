;;; init-winner.el --- Winner Mode Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file initializes and customizes `winner-mode`, allowing you to
;; navigate backward and forward through previous window configurations.

;;; Code:

;; --- 1. `winner-mode` (Window Configuration Navigation) ---
(use-package winner
  :straight nil
  :config
  (winner-mode 1)

  :custom
  ;; Define a list of "boring" buffers whose appearance/disappearance in a window
  ;; should *not* be recorded by `winner-mode`. This prevents your undo history
  ;; from being cluttered by transient buffers like completion lists or help.
  (winner-boring-buffers
   '("*Completions*"            ; Buffers for completion candidates (e.g., from Ivy, Vertico).
     "*Compile-Log*"            ; Logs from compilation commands.
     "*inferior-lisp*"          ; Interactive Lisp process buffers.
     "*Fuzzy Completions*"      ; Similar to *Completions*.
     "*Apropos*"                ; Output from apropos commands.
     "*Help*"                   ; Help buffers (e.g., from `C-h f`).
     "*cvs*"                    ; CVS interaction buffers.
     "*Buffer List*"            ; The `list-buffers` output.
     "*Ibuffer*"                ; The Ibuffer buffer.
     "*esh command on file*"    ; Output from eshell commands.
     "*Occur*"                  ; Occur mode results.
     "*grep*"                   ; Grep command output.
     "*Messages*"               ; Emacs messages buffer.
     "*scratch*"))              ; The scratch buffer (if you frequently hide/show it).

  :bind
  ;; Optional: Define keybindings for `winner-mode`.
  ;; These are common default keybindings that you might want to use or customize.
  ("C-c <left>" . winner-undo)   ; Go back to the previous window configuration.
  ("C-c <right>" . winner-redo)  ; Go forward to a later window configuration.
  )

(provide 'init-winner)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-winner.el ends here
