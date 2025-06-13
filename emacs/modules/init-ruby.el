;;; init-ruby.el --- Ruby Language Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Emacs for Ruby development, using the built-in
;; `ruby-mode`, `inf-ruby` for interactive REPL, and integrating with `lsp-mode`
;; for `solargraph` or `ruby-lsp`.

;;; Code:

;; --- 1. `ruby-mode` (Built-in Major Mode for Ruby) ---
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))

(add-hook 'ruby-mode-hook
          (lambda ()
            (setq-local ruby-indent-level 2)
            (setq-local ruby-indent-tabs-mode nil) ; Prefer spaces
            (setq-local require-final-newline t)
            ))

;; --- 2. `inf-ruby` (Interactive Ruby Shell) ---
(use-package inf-ruby
  :straight t
  :hook (ruby-mode . inf-ruby-minor-mode)
  :config
  ;; Optional: Customize behavior
  ;; (setq inf-ruby-first-line-pat "^\\(ruby\\|jruby\\|irb\\)")
  )

;; --- 2. `rubocop` (Optional: Code Formatter) ---
(use-package rubocop
  :straight t
  :hook (ruby-mode . rubocop-mode)
  :config
  (add-hook 'before-save-hook 'rubocop-format-buffer nil 'local)
  )

(provide 'init-ruby)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ruby.el ends here
