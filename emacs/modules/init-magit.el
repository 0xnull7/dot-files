;;; init-magit.el --- Configuration for Git with Magit -*- lexical-binding: t; -*-

;;; Commentary:
;; This file initializes and configures Magit, Emacs's powerful
;; Git porcelain, along with custom keybindings and helpful functions.

;;; Code:

;; --- 1. Magit (Git Interface) ---
(use-package magit
  :straight t
  :if (executable-find "git")
  :commands (magit-status)
  :bind
  ("C-x g" . magit-status)
  (:map magit-status-mode-map
        ;; Open the file associated with the diff in another window.
        ("M-RET" . magit-diff-visit-file-other-window)
        ;; Add more Magit status keybindings here if desired, e.g.:
        ;; ("s" . magit-stage-file) ; Stage current file/hunk
        ;; ("u" . magit-unstage-file) ; Unstage current file/hunk
        ;; ("c" . magit-commit) ; Start a commit
        ;; ("P" . magit-push) ; Push changes
        )
  :config
  ;; Define a convenient function to view the log of the current file with history.
  (defun magit-log-follow-current-file ()
    "View the commit log for the current file, following its history across renames.
This is a wrapper around `magit-log-buffer-file` with the `--follow` argument."
    (interactive)
    (magit-log-buffer-file t)) ; The `t` argument passes `--follow` to `git log`.

  ;; Recommended Magit settings:
  ;; Always show commit message in commit buffer (even if empty).
  (setq magit-commit-show-diff t)
  ;; Show the diff of the current commit when viewing the log.
  (setq magit-log-auto-show-diff t)
  ;; Automatically refresh status buffer (can be resource-intensive in large repos).
  ;; (setq magit-refresh-delay 0.5) ; Adjust as needed.
  )

(provide 'init-magit)
;;; init-magit.el ends here
