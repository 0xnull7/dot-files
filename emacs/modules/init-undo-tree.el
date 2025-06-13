;;; init-undo-tree.el --- Undo Tree Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file initializes and customizes `undo-tree`, providing a persistent
;; and branching undo/redo history for Emacs buffers.

;;; Code:

;; --- 1. `undo-tree` (Branching Undo/Redo History) ---
(use-package undo-tree
  :straight t
  :defer t
  :diminish undo-tree-mode

  :init
  (global-undo-tree-mode)

  :custom
  ;; Enable visual diffs in the `undo-tree-visualizer` buffer.
  ;; This helps you see exactly what changed at each step in the history.
  (undo-tree-visualizer-diff t)
  ;; Store undo history in a dedicated `.backup` directory within your Emacs config.
  ;; This ensures undo history persists across Emacs sessions.
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name ".backup" user-emacs-directory))))
  ;; Display timestamps in the `undo-tree-visualizer` to see when changes were made.
  (undo-tree-visualizer-timestamps t)

  :bind
  ;; Optional: Add keybindings for `undo-tree` if you want to override defaults
  ;; or add custom shortcuts. For example:
  ;; ("C-x u" . undo-tree-visualize) ; Visualize the undo tree
  ;; ("C-z"   . undo-tree-undo)      ; Undo
  ;; ("C-S-z" . undo-tree-redo)      ; Redo (often bound to C-y or similar)
  )

(provide 'init-undo-tree)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-undo-tree.el ends here
