;;; init-go.el --- Configuration for Go Development -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures the development environment for Go with a
;; strong focus on LSP integration via lsp-mode and goimports for formatting.

;;; Code:

(use-package go-mode
  :straight t
  :mode "\\.go\\'"
  :hook (go-mode . lsp-deferred)
  :config
  ;; Use `goimports` for automatic import management and formatting on save.
  ;; Ensure `goimports` is installed and available in your system's PATH.
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save nil t)

  ;; Recommended additional Go-specific settings:
  ;; Automatically run `go-rename-buffer` when `go-mode` is active.
  ;; This renames the Emacs buffer to match the Go package name.
  (add-hook 'go-mode-hook #'go-rename-buffer-on-activate)

  ;; Enable Go-specific indentation settings.
  (setq tab-width 4
        indent-tabs-mode t)
  ;; Other Go-specific mode settings can go here.
  )

;; If you use `go-mode` for other utilities beyond core editing,
;; consider binding them here. For example, for running tests:
;; (use-package go-mode
;;   :bind (:map go-mode-map
;;                ("C-c r" . go-run)
;;                ("C-c t" . go-test)))

(provide 'init-go)
;;; init-go.el ends here
