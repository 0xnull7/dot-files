;;; init-vlang.el --- V Language Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Emacs for V language development, using `v-mode`
;; and integrating with `lsp-mode` for `vls` (V Language Server).

;;; Code:

;; --- 1. `v-mode` (Major Mode for V Language) ---
(use-package v-mode
  :straight t
  :mode "\\.v\\'"
  :config
  (setq v-indent-offset 4)
  (setq v-indent-tabs-mode nil) ; Prefer spaces
  )

(provide 'init-vlang)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vlang.el ends here
