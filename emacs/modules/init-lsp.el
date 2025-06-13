;;; init-lsp.el --- Configuration for LSP and DAP -*- lexical-binding: t; -*-

;;; Commentary:
;; This file initializes `lsp-mode` for Language Server Protocol support,
;; `lsp-ui` for enhanced UI, and `dap-mode` for debugging within Emacs.

;;; Code:

;; --- 1. `lsp-mode` (Core Language Server Protocol Client) ---
(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook
  ((LaTeX-mode ada-mode c++-mode c-mode clojure-mode cmake-mode
    cperl-mode csharp-mode d-mode gdscript-mode go-mode java-mode
    js-mode js2-mode kotlin-mode markdown-mode nim-mode objc-mode
    pascal-mode php-mode powershell-mode python-mode ruby-mode
    rust-mode sh-mode sql-mode toml-mode typescript-mode v-mode
    web-mode yaml-mode zig-mode) . lsp-deferred)
  :custom
  ;; Keymap prefix for LSP-related commands.
  (lsp-keymap-prefix "C-x l")
  ;; Do not automatically guess project root; rely on LSP server detection or manual setup.
  (lsp-auto-guess-root nil)
  ;; Prefer `flycheck` for syntax checking over `flymake`.
  (lsp-prefer-flymake nil)
  ;; Disable LSP file watchers; often resource-intensive, and many servers handle this internally.
  (lsp-enable-file-watchers nil)
  ;; Disable LSP code folding; consider `origami.el` or `hideshow` for folding if needed.
  (lsp-enable-folding nil)
  ;; Increase the maximum process output read size to handle large LSP responses.
  (read-process-output-max (* 1024 1024)) ; 1MB
  ;; Keep LSP workspace alive even if all buffers are closed.
  (lsp-keep-workspace-alive nil)
  ;; Do not use `eldoc-mode` with LSP; `lsp-ui-doc` often provides better information.
  (lsp-eldoc-hook nil)
  ;; Set the default formatter to `lsp-format-buffer`.
  ;; This ensures `indent-buffer` or `C-c C-f` uses the LSP formatter.
  (lsp-format-buffer-on-save t) ; Auto-format on save
  (format-all-buffer t)        ; Ensure `format-all-buffer` is also set if used.
  (lsp-enable-completion-at-point nil) ; Disable built-in completion if you use `company-mode` exclusively.
  (setq eldoc-documentation-functions nil)

  :bind
  ;; Global LSP keybindings under the `lsp-mode-map`
  (:map lsp-mode-map
        ("C-c C-f" . lsp-format-buffer) ; Manual formatting.
        ("M-n" . lsp-goto-next-error)   ; Navigate diagnostics (better than paragraph movement).
        ("M-p" . lsp-goto-prev-error))  ; Navigate diagnostics.

  :config
  ;; Function to update LSP servers (install new ones or update existing ones).
  (defun lsp-update-server ()
    "Update LSP server. This is equivalent to `C-u M-x lsp-install-server'."
    (interactive)
    (lsp-install-server t))) ; `t` forces a reinstallation/update.

;; --- 2. `lsp-ui` (LSP User Interface Enhancements) ---
(use-package lsp-ui
  :straight t        ; Ensure `lsp-ui` is managed and installed by straight.el.
  :after lsp-mode    ; Load `lsp-ui` only after `lsp-mode` is available.
  :diminish lsp-ui-mode ; Hide `LspUi` from the mode line for a cleaner look.
  :commands lsp-ui-mode ; Command to explicitly enable `lsp-ui-mode`.
  :custom-face
  ;; Customize the appearance of LSP documentation popups.
  (lsp-ui-doc-background ((t (:background nil)))) ; Transparent background.
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic))))) ; Header style.
  :custom
  (lsp-ui-doc-header t)                 ; Show header in documentation popups.
  (lsp-ui-doc-include-signature t)      ; Include function signatures.
  (lsp-ui-doc-border (face-foreground 'default)) ; Border color matches foreground.
  (lsp-ui-sideline-enable nil)          ; Disable sidebar diagnostics (can be noisy).
  (lsp-ui-sideline-ignore-duplicate t)  ; Ignore duplicate sideline messages.
  (lsp-ui-sideline-show-code-actions nil) ; Don't show code actions in the sideline.

  :bind
  ;; Custom keybindings for `lsp-ui-mode`.
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions) ; Peek definitions.
        ([remap xref-find-references] . lsp-ui-peek-find-references)   ; Peek references.
        ("C-c u" . lsp-ui-imenu) ; Show outline/symbols using `lsp-ui-imenu`.
        ("M-i" . lsp-ui-doc-focus-frame)) ; Focus documentation frame.

  :config
  ;; Use `webkit` for richer documentation rendering in graphical Emacs.
  (when (display-graphic-p)
    (setq lsp-ui-doc-use-webkit t))

  ;; WORKAROUND: Hide mode-line of the `lsp-ui-imenu` buffer.
  ;; This is an advice to remove the mode line specifically for this popup.
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq-local mode-line-format nil)) ; Use `setq-local` for buffer-local change.

  ;; Advise `keyboard-quit` to hide LSP UI documentation popups.
  ;; This ensures `C-g` closes the documentation first.
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))

;; --- 3. `dap-mode` (Debugger Adapter Protocol Client) ---
(use-package dap-mode
  :straight t        ; Ensure `dap-mode` is managed and installed by straight.el.
  :after lsp-mode    ; Load `dap-mode` after `lsp-mode` (often used together).
  :diminish dap-mode ; Hide `Dap` from the mode line.
  :hook (dap-mode . dap-tooltip-mode) ; Enable tooltips when `dap-mode` is active.
  :bind
  ;; Global keybindings for debugging commands.
  (:map dap-mode-map
        ("<f12>"     . dap-debug)            ; Start/Restart debugging.
        ("<f8>"      . dap-continue)         ; Continue execution.
        ("<f9>"      . dap-next)             ; Step over.
        ("<M-f11>"   . dap-step-in)          ; Step into.
        ("C-M-<f11>" . dap-step-out)         ; Step out.
        ("<f7>"      . dap-breakpoint-toggle)) ; Toggle breakpoint at current line.
  :config
  ;; Custom DAP-related settings can go here, e.g., for specific debuggers.
  ;; (require 'dap-go) ; Example for Go debugger.
  ;; (require 'dap-python) ; Example for Python debugger.
  )

;; --- 4. `lsp-treemacs` (Integration with Treemacs for LSP) ---
(use-package lsp-treemacs
  :straight t        ; Ensure `lsp-treemacs` is managed and installed by straight.el.
  :commands lsp-treemacs-errors-list ; Command to display LSP errors in Treemacs.
  :after (lsp-mode treemacs) ; Load after `lsp-mode` and `treemacs`.
  ;; You might want to bind `lsp-treemacs-errors-list` to a key here.
  ;; :bind ("C-x l e" . lsp-treemacs-errors-list)
  )

(provide 'init-lsp)
;;; init-lsp.el ends here
