;;; init-csharp-dotnet.el --- C# and .NET Development Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Emacs for C# and .NET development, leveraging
;; `csharp-mode` and robust LSP integration via `lsp-mode` with `csharp-ls`
;; or `omnisharp-roslyn`.

;;; Code:

;; --- 1. `csharp-mode` (Major Mode for C#) ---
(use-package csharp-mode
  :straight t
  :mode "\\.cs\\'"
  :config
  (setq csharp-indent-offset 4)
  (setq csharp-tab-always-indent t)
  )

  ;; Optional: Customize LSP settings for C#
  ; (lsp-register-client
  ;  (make-lsp-client :new-connection (lsp-stdio-connection "csharp-ls")
  ;                   :major-modes '(csharp-mode)
  ;                   :priority 100
  ;                   :server-id 'csharp-ls))
  ; )

;; --- 2. `project.el` integration (built-in) ---
;; For .NET projects, `project.el` can integrate with `dotnet` CLI.
;; No specific `use-package` needed for `project.el`.
(add-hook 'csharp-mode-hook
          (lambda ()
            (add-to-list 'project-vc-known-root-markers "Solution.sln" t) ; Mark .sln as project root
            (add-to-list 'project-vc-known-root-markers ".csproject" t)
            (add-to-list 'project-vc-known-root-markers ".git" t)
            (add-to-list 'project-vc-known-root-markers ".vscode" t)))

(provide 'init-csharp-dotnet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-csharp-dotnet.el ends here
