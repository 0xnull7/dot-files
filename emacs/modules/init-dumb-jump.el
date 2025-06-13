;;; init-dumb-jump.el --- Jump to definitions/references -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; This file configures `dumb-jump` for "jump to definition" functionality.
;; It uses various backend tools (like `grep`, `ack`, `ag`, `rg`) to locate definitions.
;;
;;; Code:

(use-package dumb-jump
  :straight t ; Ensure straight.el manages this package.
  :defer t    ; Explicitly defer loading until a command is called.
  :init
  ;; Set the preferred selector early. If `ivy` is not available, `dumb-jump` will fall back.
  (setq dumb-jump-selector 'ivy)
  ;; Ensure an external backend is available. `ripgrep` is highly recommended.
  ;; If `ripgrep` isn't found, it will try `ag`, `ack`, `grep`, in that order.
  (setq dumb-jump-default-searcher 'rg)

  :bind
  ;; Bindings are specific to `prog-mode-map` for context-aware usage.
  (:map prog-mode-map
        ("C-c C-o" . dumb-jump-go-other-window) ; Jump to definition in another window
        ("C-c C-j" . dumb-jump-go)             ; Jump to definition in the current window
        ("C-c C-i" . dumb-jump-go-prompt)      ; Prompt for a definition to jump to
        ;; Consider adding bindings for `dumb-jump-back` and `dumb-jump-quick-look`.
        ;; ("C-c C-b" . dumb-jump-back)
        ;; ("C-c C-q" . dumb-jump-quick-look)
        )

  :config
  ;; Additional `dumb-jump` configurations can go here.
  ;; For example, you might want to customize the ignored directories:
  ;; (add-to-list 'dumb-jump-ignored-directories "node_modules")
  ;; (add-to-list 'dumb-jump-ignored-directories ".git")
  )

(provide 'init-dumb-jump)
;;; init-dumb-jump.el ends here
