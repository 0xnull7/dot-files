;;; init-sql.el --- SQL Language Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Emacs for SQL development, using the built-in
;; `sql-mode` and optionally integrating with an SQL language server.

;;; Code:

;; --- 1. `sql-mode` (Built-in Major Mode for SQL) ---
(add-to-list 'auto-mode-alist '("\\.sql\\'" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.ddl\\'" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.dml\\'" . sql-mode))

(add-hook 'sql-mode-hook
          (lambda ()
            (setq-local sql-indent-offset 4)
            (setq-local sql-indent-tabs-mode nil) ; Prefer spaces
            (setq-local comment-start "-- ")
            (setq-local comment-end "")
            (setq-local require-final-newline t)
            ))

;; --- 2. `sql-indent` (Optional: Advanced SQL Indentation) ---
(use-package sql-indent
  :straight t
  :hook (sql-mode . sql-indent-mode)
  :config
  ;; Optional: Customize `sql-indent` settings
  ;; (setq sql-indent-line-threshold 10)
  )

(provide 'init-sql)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-sql.el ends here
