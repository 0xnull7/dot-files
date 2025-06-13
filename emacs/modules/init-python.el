;;; init-python.el --- Python Development Environment Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures Emacs for Python development, including basic Python mode
;; settings and integration with the Pyright Language Server for advanced features.

;;; Code:

;; --- 1. `python-mode` (Basic Python Editing) ---
;; `python-mode` is built-in and not installed via `straight.el`.
(use-package python-mode
  :straight nil
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4)
  (flycheck-python-pycompile-executable "python3")
  (python-shell-interpreter "python3")

  :config
  ;; You might want to add hooks for `flycheck` or other minor modes here.
  ;; Example: Enable `flycheck` in Python buffers.
  ;; (add-hook 'python-mode-hook #'flycheck-mode)
  ;; Example: Enable `electric-pair-mode` for automatic parenthesis pairing.
  ;; (add-hook 'python-mode-hook #'electric-pair-mode)
  )

;; --- 2. `lsp-pyright` (Pyright Language Server for Python) ---
(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred)))
  :custom
  ;; Disable multi-root workspace support if you primarily work with single-folder projects.
  ;; This can simplify setup for Pyright.
  (lsp-pyright-multi-root nil)

  :config
  (require 'lsp)
  ;; You can add custom Pyright settings here, for example:
  ;; (setq lsp-pyright-executable "/usr/local/bin/pyright-langserver") ; Specify Pyright executable path.
  ;; (add-to-list 'lsp-language-id-configuration '(python-mode . "python")) ; Ensure language ID is correct.
  )

(provide 'init-python)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
