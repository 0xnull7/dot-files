;;; init-markdown.el --- Configuration for Markdown and GFM -*- lexical-binding: t; -*-

;;; Commentary:
;; This file initializes `markdown-mode` for general Markdown editing,
;; `gfm-mode` for GitHub Flavored Markdown, `markdown-toc` for TOC generation,
;; `markdownfmt` for auto-formatting, and `lsp-marksman` for LSP features.

;;; Code:

;; --- 1. `markdown-mode` & `gfm-mode` (Core Markdown Support) ---
(use-package markdown-mode
  :straight t
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  :hook
  (markdown-mode . lsp-deferred)
  :config
  ;; You can add custom Markdown-specific settings here, e.g.:
  (setq markdown-italic-syntax 'asterisk) ; Use asterisks for italics.
  (setq markdown-fontify-code-blocks-natively t) ; Native fontification for code blocks.
  ;; (setq markdown-hide-markup t) ; Hide markup characters for cleaner display.
  )

;; --- 2. `markdown-toc` (Table of Contents Generation) ---
(use-package markdown-toc
  :straight t
  :after markdown-mode
  :commands (markdown-toc-insert-or-update)
  :bind (:map markdown-mode-map ("C-c t" . markdown-toc-insert-or-update))
  )

;; --- 3. `markdownfmt` (Markdown Auto-formatter) ---
(use-package markdownfmt
  :straight t
  :after markdown-mode
  :hook (markdown-mode . markdownfmt-enable-on-save)
  :config
  ;; Ensure `markdownfmt` executable is installed and in your system's PATH.
  ;; (setq markdownfmt-executable "/path/to/your/markdownfmt")
  )

(provide 'init-markdown)
;;; init-markdown.el ends here
