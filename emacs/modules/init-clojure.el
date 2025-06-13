;;; init-clojure.el --- Clojure Language Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Emacs for Clojure development, primarily using
;; `clojure-mode` for syntax highlighting and `cider` for interactive
;; development, completion, and debugging.

;;; Code:

;; --- 1. `clojure-mode` (Major Mode for Clojure) ---
(use-package clojure-mode
  :straight t
  :mode "\\.clj[cs]?\\'"
  :config
  (setq clojure-indent-offset 4)
  (setq clojure-align-forms-automatically t)
  )

;; --- 2. `cider` (Clojure Interactive Development Environment) ---
(use-package cider
  :straight t
  :hook (clojure-mode . cider-mode)
  :config
  ;; Optional: Customize cider behavior
  (setq cider-auto-select-profile t) ; Auto-select Leiningen/deps.edn profiles
  (setq cider-repl-display-help-at-start nil) ; Don't show help on REPL start
  (setq cider-font-lock-dynamically t) ; Dynamic font-lock (more accurate, slightly slower)

  ;; Keybindings for common Cider commands (you might want to bind these globally or in a prefix)
  ;; (define-key clojure-mode-map (kbd "C-c C-k") #'cider-load-buffer)
  ;; (define-key clojure-mode-map (kbd "C-c C-e") #'cider-eval-last-sexp)
  ;; (define-key clojure-mode-map (kbd "C-c C-z") #'cider-switch-to-repl-buffer)
  )

(provide 'init-clojure)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-clojure.el ends here
