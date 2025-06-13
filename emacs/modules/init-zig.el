;;; init-zig.el --- Zig Language Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Emacs for Zig development, using `zig-mode`
;; and integrating with `lsp-mode` for `zls` (Zig Language Server).

;;; Code:

;; --- 1. `zig-mode` (Major Mode for Zig) ---
(use-package zig-mode
  :straight t
  :mode "\\.zig\\'"
  :config
  (setq zig-indent-offset 4)
  (setq zig-indent-tabs-mode nil) ; Prefer spaces
  )

;; --- 2. Optional: Formatting (`zig fmt`) ---
;; Zig has a built-in formatter `zig fmt`. You can integrate it via a minor mode.
;; (use-package reformatter ; Example for external formatter integration
;;   :straight t
;;   :hook (zig-mode . reformatter-auto-mode)
;;   :config
;;   (reformatter-define zig-fmt
;;     :program "zig"
;;     :args '("fmt" "-")) ; `zig fmt -` reads from stdin, writes to stdout
;;   )

(provide 'init-zig)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-zig.el ends here
