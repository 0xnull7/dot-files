;;; init-haskell.el --- Configuration for Haskell Development -*- lexical-binding: t; -*-

;;; Commentary:
;; This file initializes `haskell-mode` and `lsp-haskell` for a rich
;; Haskell development environment in Emacs, including LSP support.

;;; Code:

;; --- 1. Haskell Mode (Core Language Support) ---
(use-package haskell-mode
  :straight t
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.lhs\\'" . haskell-literate-mode))
  ;; You might want to add basic Haskell-specific settings here, e.g.:
  :config
  ;; (setq haskell-stylish-on-save t) ; If you use `stylish-haskell` for formatting.
  ;; (add-hook 'haskell-mode-hook #'haskell-indent-mode) ; Ensure indentation is active.
  )

;; --- 2. LSP Haskell (Language Server Protocol Integration) ---
(use-package lsp-haskell
  :straight t ; Ensure `lsp-haskell` is managed and installed by straight.el.
  :hook ((haskell-mode haskell-literate-mode) . lsp-deferred) ; Enable `lsp-mode` for Haskell buffers.
  :config
  ;; Specify the path to your `haskell-language-server` executable.
  ;; Ensure `haskell-language-server` is installed on your system and in your PATH.
  (setq lsp-haskell-server-path "haskell-language-server")
  ;; Other LSP-related Haskell configurations can go here.
  ;; (setq lsp-haskell-process-args '("--ignore-dir=dist-newstyle"))
  )

(provide 'init-haskell)
;;; init-haskell.el ends here
