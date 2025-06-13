;;; init-gdscript.el --- GDScript Language Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Emacs for GDScript development (Godot Engine's scripting language),
;; using `gdscript-mode` for syntax highlighting and basic features.

;;; Code:

;; --- 1. `gdscript-mode` (Major Mode for GDScript) ---
(use-package gdscript-mode
  :straight t
  :mode "\\.gd\\'"
  :config
  ;; Basic GDScript indentation. GDScript uses 4 spaces.
  (setq gdscript-indent-offset 4)
  (setq gdscript-indent-tabs-mode nil) ; Prefer spaces
  )

(provide 'init-gdscript)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-gdscript.el ends here
