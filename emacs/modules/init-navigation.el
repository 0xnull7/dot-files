;;; init-navigation.el --- Navigation and Help Utilities Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures packages for improved navigation, search, and help functionalities.

;;; Code:

;; --- 1. `goto-line-preview` (Interactive Line Navigation) ---
(use-package goto-line-preview
  :straight t
  :commands (goto-line-preview)
  :bind
  ([remap goto-line] . goto-line-preview))

;; --- 2. `helpful` (Enhanced Help Functions) ---
(use-package helpful
  :straight t
  :commands (helpful-callable helpful-variable helpful-key helpful-command)
  :bind
  ("C-h f" . helpful-callable) ; Help on functions (including macros and interactive commands).
  ("C-h v" . helpful-variable) ; Help on variables.
  ("C-h k" . helpful-key)      ; Help on keybindings.
  ("C-h x" . helpful-command)  ; Help on interactive commands.
  ("C-h F" . helpful-function) ; More direct help on functions (not necessarily interactive).
  ("C-h C" . helpful-at-point) ; General help for symbol at point.
  )

(provide 'init-navigation)
;;; init-navigation.el ends here
