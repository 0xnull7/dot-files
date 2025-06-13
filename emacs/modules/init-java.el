;;; init-java.el --- Configuration for Java Development -*- lexical-binding: t; -*-

;;; Commentary:
;; This file initializes and configures `lsp-java` for a robust Java
;; development environment in Emacs, leveraging the Language Server Protocol.
;; `dap-mode` for debugging, and sets up general Java-specific Emacs behaviors.

;;; Code:

;; Set basic indentation and tab width for Java globally as defaults.
;; These can be overridden by specific mode-hooks if needed, but provide a good baseline.
(setq-default java-basic-offset 4)
(setq-default tab-width 4)

;; 1. `lsp-java` (Java Language Server Protocol Integration)
;; This is an external package, so 'use-package' with ':straight t' is correct.
(use-package lsp-java
  :straight t
  :after lsp-mode ; Ensure lsp-mode is loaded before configuring lsp-java
  :hook (java-mode . lsp-deferred) ; Defer LSP activation until needed in Java buffers
  :init
  (use-package request :straight t :defer t) ; 'request' is a dependency for lsp-java
  :if (executable-find "mvn") ; Conditionally load if 'mvn' (Maven) is found
  :custom
  ;; Define directories for LSP Java server installation and workspace.
  ;; These paths use 'locate-user-emacs-file' to keep them within your Emacs config.
  (lsp-java-server-install-dir (locate-user-emacs-file "eclipse.jdt.ls/server/"))
  (lsp-java-workspace-dir (locate-user-emacs-file "eclipse.jdt.ls/workspace/"))
  ;; Customize the Java Language Server's log level (optional)
  ;; (lsp-java-trace-server "verbose")
  )


; (use-package dap-java :after dap-mode)

;; 3. Code Formatting with LSP
;; lsp-mode provides a command to format the current buffer using the LSP server.
(add-hook 'java-mode-hook
          (lambda ()
            ;; Bind a key to format the buffer using LSP.
            ;; 'C-c f' is a common prefix for formatting.
            (local-set-key (kbd "C-c f") 'lsp-format-buffer)
            ;; You might also want to set it to format on save, but be cautious as this
            ;; can sometimes be aggressive.
            ;; (add-hook 'before-save-hook 'lsp-format-buffer nil t)
            ))

;; 4. `imenu` for Code Outline/Navigation (Built-in)
;; 'imenu' is a built-in Emacs feature that creates a menu of major definitions
;; in the current buffer. It's great for navigating large Java files.
;; You can access it via 'M-x imenu' or by calling 'imenu-tree' (if you have that package).
(add-hook 'java-mode-hook
          (lambda ()
            (imenu-add-to-menubar)
            ))

(provide 'init-java)
;;; init-java.el ends here
