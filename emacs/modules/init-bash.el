;;; init-bash.el --- Bash Scripting Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Emacs for Bash scripting, using the built-in `sh-mode`
;; and integrating with `lsp-mode` for `bash-language-server`.

;;; Code:

;; --- 1. `sh-mode` (Built-in Shell Script Mode) ---
;; `sh-mode` is built-in and handles various shell dialects including Bash.
(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("/bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("/profile\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("/zshrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("/.bash_profile\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("/.bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("/.zshrc\\'" . sh-mode))

(add-hook 'sh-mode-hook
          (lambda ()
            ;; Set indentation specific to shell scripts
            (setq-local sh-basic-offset 2)
            (setq-local sh-indentation 2)
            (setq-local sh-indent-after-continuation nil) ; Don't indent after backslash
            (setq-local sh-electric-indent t) ; Enable electric indentation
            (setq-local comment-start "# ")
            (setq-local comment-end "")
            (setq-local require-final-newline t)
            ))

(provide 'init-bash)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-bash.el ends here
