;;; init-rust.el --- Configuration for Rust -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures the development environment for Rust, with a
;; strong focus on LSP integration via `lsp-mode` and `rustic`.

;;; Code:

;; --- 1. `rustic` (Full-featured Rust Development Environment) ---
(use-package rustic
  :straight t
  :mode "\\.rs\\'"
  :hook
  (rustic-mode . (lambda ()
                   (lsp-deferred)
                   (cargo-minor-mode)))

  :custom
  (rustic-indent-offset 4)
  (rustic-format-on-save t)
  (rustic-cargo-use-last-stored-arguments t)
  ;; (rustic-diagnostics-display-style 'mixed) ; How diagnostics are displayed.
  )

(use-package toml-mode
  :straight t
  :mode "\\.toml\\'"
  :config
  (setq toml-indent-offset 2)
  (setq toml-basic-offset 2)
  (setq toml-indent-tabs-mode nil) ; Prefer spaces
  )

(provide 'init-rust)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rust.el ends here
