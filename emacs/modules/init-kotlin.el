;;; init-kotlin.el --- Kotlin Language Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Emacs for Kotlin development, using `kotlin-mode`
;; and integrating with `lsp-mode` for `kotlin-language-server`.

;;; Code:

;; --- 1. `kotlin-mode` (Major Mode for Kotlin) ---
(use-package kotlin-mode
  :straight t
  :mode "\\.kt\\'"
  :config
  ;; Basic Kotlin indentation. Kotlin typically uses 4 spaces.
  (setq kotlin-indent-offset 4)
  (setq kotlin-tab-width 4)
  (setq kotlin-indent-tabs-mode nil)
  )

;; --- 2. Optional: Formatting (`ktlint`) ---
;; If you use `ktlint` for formatting, you can integrate it via `dap-mode` or `prettify-symbols`.
;; (use-package reformatter ; Example for external formatter integration
;;   :straight t
;;   :hook (kotlin-mode . reformatter-auto-mode)
;;   :config
;;   (reformatter-define ktlint
;;     :program "ktlint"
;;     :args '("--stdin" "--fmt"))
;;   )

(provide 'init-kotlin)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-kotlin.el ends here
