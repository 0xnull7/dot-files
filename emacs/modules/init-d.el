;;; init-d.el --- D Language Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Emacs for D language development, using `d-mode`
;; and integrating with `lsp-mode` for `dls` (D Language Server).

;;; Code:

;; --- 1. `d-mode` (Major Mode for D Language) ---
(use-package d-mode
  :straight t
  :mode "\\.d\\'"
  :config
  ;; Basic D indentation
  (setq d-indent-offset 4)
  (setq d-indent-tabs-mode nil) ; Prefer spaces
  )

(provide 'init-d)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-d.el ends here
