;;; init-ada.el --- Ada Language Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Emacs for Ada development, including `ada-mode`
;; and integration with `lsp-mode` for language server support.

;;; Code:

;; --- 1. `ada-mode` (Major Mode for Ada) ---
(use-package ada-mode
  :straight t ; Ensure `ada-mode` is installed by straight.el
  :mode "\\.ad[sb]\\'" ; Associate with .ads (spec) and .adb (body) files
  :config
  ;; Basic Ada indentation. Adjust as needed.
  (setq ada-indent-level 3)
  (setq ada-indent-in-comments t)
  (setq ada-indent-with-tabs nil) ; Prefer spaces for indentation
  )

(provide 'init-ada)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ada.el ends here
