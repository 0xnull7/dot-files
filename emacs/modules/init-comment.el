;;; init-comment.el --- Configuration for code commenting -*- lexical-binding: t; -*-

;;; Commentary:
;; Uses `evil-nerd-commenter` to provide powerful and consistent line/region
;; commenting commands. This package works with or without `evil-mode`.

;;; Code:

(use-package evil-nerd-commenter
  :straight t
  :defer t
  :bind (
         ("M-;" . evilnc-comment-or-uncomment-lines)
         ;; An extra binding for toggling C-style /* ... */ block comments.
         ;; Note: This command is specific to `c-mode` and related modes.
         ;; Binding it globally might not have the desired effect outside those modes.
         ;; Consider binding it to a mode-specific map if you want more control.
         ("C-c M-;" . c-toggle-comment-style)
         ;; Consider adding other useful evil-nerd-commenter commands, e.g.:
         ;; ("C-x /" . evilnc-comment-or-uncomment-sexp)
         ;; ("C-c c c" . evilnc-comment-or-uncomment-region)
         ))

(provide 'init-comment)
;;; init-comment.el ends here
