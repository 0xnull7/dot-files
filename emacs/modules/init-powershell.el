;;; init-powershell.el --- PowerShell Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Emacs for PowerShell development, using `powershell-mode`
;; and integrating with `lsp-mode` for `powershell-editor-services`.

;;; Code:

;; --- 1. `powershell-mode` (Major Mode for PowerShell) ---
(use-package powershell-mode
  :straight t
  :mode "\\.ps1\\'"
  :config
  (setq powershell-indent-offset 4)
  (setq powershell-indent-tabs-mode nil) ; Prefer spaces
  )

(provide 'init-powershell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-powershell.el ends here
