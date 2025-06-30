;;; init-nim.el --- Nim Language Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Emacs for Nim development, using `nim-mode`
;; and integrating with `lsp-mode` for `nimlsp`.

;;; Code:

;; --- 1. `nim-mode` (Major Mode for Nim) ---
(use-package nim-mode
  :straight t
  :mode "\\.nim\\'"
  :config
  ;; Basic Nim indentation. Nim uses 2 spaces.
  (setq nim-indent-offset 4)
  (setq nim-indent-tabs-mode nil) ; Prefer spaces
  )

(provide 'init-nim)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-nim.el ends here
