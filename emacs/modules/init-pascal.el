;;; init-pascal.el --- Pascal Language Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Emacs for Pascal, Object Pascal, Free Pascal, and Delphi
;; development, primarily using `pascal-mode` for syntax highlighting and basic features.
;; Advanced LSP integration is less common for Pascal in Emacs compared to other languages.

;;; Code:

;; --- 1. `pascal-mode` (Major Mode for Pascal) ---

;; Associate Pascal file extensions with pascal-mode.
;; This ensures that files with these extensions automatically open in pascal-mode.
(add-to-list 'auto-mode-alist '("\\.pas\\'" . pascal-mode))    ; Pascal, Free Pascal
(add-to-list 'auto-mode-alist '("\\.pp\\'" . pascal-mode))     ; Free Pascal program units
(add-to-list 'auto-mode-alist '("\\.dpk\\'" . pascal-mode))    ; Delphi Package files
(add-to-list 'auto-mode-alist '("\\.lpr\\'" . pascal-mode))    ; Lazarus Project files
(add-to-list 'auto-mode-alist '("\\.dpr\\'" . pascal-mode))    ; Delphi Project files

;; Configure pascal-mode settings when the mode is enabled in a buffer.
;; Using a lambda with 'add-hook' ensures these settings are buffer-local.
(add-hook 'pascal-mode-hook
          (lambda ()
            ;; Set indentation offset for Pascal code.
            (setq-local pascal-indent-offset 4)
            ;; Prefer spaces over tabs for indentation.
            (setq-local pascal-indent-tabs-mode nil)
            ;; Enable automatic indentation as you type.
            (setq-local pascal-auto-indent t)

            ;; Optional: Customize comment style if needed for specific Pascal dialects.
            ;; Uncomment and adjust if your Pascal dialect uses different comment delimiters.
            ;; (setq-local comment-start "{")
            ;; (setq-local comment-end "}")
            ))

(provide 'init-pascal)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-pascal.el ends here
