;;; init-cc.el --- Configuration for C/C++ -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Configures the development environment for C/C++, with a
;; strong focus on LSP integration via lsp-mode, along with
;; indentation, and common C/C++ development utilities.

;;; Code:

;;;; --- C / C++ ---
;; 1. Core Mode Hooks for LSP and Other Features
;; These modes (c-mode, c++-mode, etc.) are built-in, so we add hooks directly.
(add-hook 'c-mode-hook 'lsp-deferred)   ; Enable LSP for C files
(add-hook 'c++-mode-hook 'lsp-deferred) ; Enable LSP for C++ files
(add-hook 'objc-mode-hook 'lsp-deferred) ; Enable LSP for Objective-C files
(add-hook 'cuda-mode-hook 'lsp-deferred) ; Enable LSP for CUDA files

;; Add more common C/C++ file extensions to be recognized by c++-mode.
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))

;; 2. Modern C++ Font Locking
;; This is an external package for enhanced font-locking, correctly using use-package.
(use-package modern-cpp-font-lock
  :straight t
  :diminish modern-c++-font-lock-mode
  :hook (c++-mode . modern-c++-font-lock-mode))

;; 3. C/C++ Indentation and Formatting Settings
;; These settings are applied when c-mode or c++-mode is active.
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Basic offset for indentation
            (setq-local c-basic-offset 4)
            ;; Prefer spaces over tabs for indentation
            (setq-local c-indent-tabs-mode nil)
            ;; Set the default C/C++ style. Common choices: "gnu", "k&r", "bsd", "stroustrup"
            (setq-local c-default-style "astyle")
            ;; Do not automatically insert newlines for closing braces
            (setq-local c-toggle-auto-newline nil)
            ;; Enable auto-indentation when typing
            (c-toggle-auto-state 1)
            ))

;; 6. Utility for Inserting Header Guards
;; A simple Elisp function and keybinding to quickly insert a header guard.
(defun my-insert-header-guard ()
  "Insert C/C++ header guard based on the current file name."
  (interactive)
  (let* ((filename (file-name-nondirectory buffer-file-name))
         (guard-name (replace-regexp-in-string "\\W+" "_" (upcase (file-name-sans-extension filename)))))
    (goto-char (point-min))
    (insert (format "#ifndef %s\n#define %s\n\n" guard-name guard-name))
    (goto-char (point-max))
    (insert (format "\n#endif // %s\n" guard-name))
    (goto-char (point-min))))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Bind F5 to insert header guard in C/C++ modes
            (local-set-key (kbd "<f5>") 'my-insert-header-guard)))

(provide 'init-cc)
;;; init-cc.el ends here
