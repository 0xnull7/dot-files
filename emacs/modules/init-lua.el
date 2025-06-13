;;; init-lua.el --- Configuration for Lua Development -*- lexical-binding: t; -*-

;;; Commentary:
;; This file initializes `lua-mode` for basic Lua syntax highlighting
;; and editing, and sets up for optional LSP integration.

;;; Code:

;; --- 1. `lua-mode` (Core Lua Language Support) ---
(use-package lua-mode
  :straight t
  :mode "\\.lua\\'"
  :config
  (setq lua-indent-level 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil) ; Prefer spaces over tabs

  (add-hook 'lua-mode-hook #'lsp-deferred)
  )

;; --- 2. Optional: LSP for Lua (Requires `lsp-mode` and a Lua Language Server) ---
;; If you decide to use LSP for Lua, you'd configure `lsp-lua` or a similar package here.
;; Example (assuming `sumneko-lua-language-server` is installed and in your PATH):
;; (use-package lsp-lua
;;   :straight t
;;   :after lsp-mode
;;   :hook (lua-mode . lsp-deferred)
;;   :custom
;;   ;; Point to your Lua language server executable if it's not in PATH
;;   ;; (lsp-lua-server-path "/path/to/lua-language-server")
;;   (lsp-lua-workspace-folders '(".")) ; Or configure specific workspace folders
;;   )

(provide 'init-lua)
;;; init-lua.el ends here
