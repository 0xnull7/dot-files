;;; init-php.el --- PHP Language Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Emacs for PHP development, using the built-in
;; `php-mode` and integrating with `lsp-mode` for a PHP language server
;; (e.g., `intelephense` or `php-language-server`).

;;; Code:

;; --- 1. `php-mode` (Built-in Major Mode for PHP) ---
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . php-mode)) ; Also covered by web-mode

(add-hook 'php-mode-hook
          (lambda ()
            (setq-local php-indent-offset 4)
            (setq-local php-tab-width 4)
            (setq-local indent-tabs-mode nil) ; Prefer spaces
            (setq-local require-final-newline t)
            ))

;; --- 2. `php-cs-fixer` (Optional: Code Formatter) ---
;; Integrate PHP-CS-Fixer for automatic formatting.
; (use-package php-cs-fixer
;   :straight t
;   :hook (php-mode . php-cs-fixer-on-save-mode) ; Format on save
;   :config
;   ;; You need to install `php-cs-fixer` separately (e.g., via Composer).
;   ;; (setq php-cs-fixer-executable "/usr/local/bin/php-cs-fixer")
;   )

(provide 'init-php)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-php.el ends here
