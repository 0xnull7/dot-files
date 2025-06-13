;;; init-elisp.el --- Emacs Lisp Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Emacs for Emacs Lisp development, leveraging
;; the built-in `emacs-lisp-mode` and adding tools for formatting and checking.

;;; Code:

;; --- 1. `emacs-lisp-mode` (Built-in Mode for Emacs Lisp) ---
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Set indentation
            (setq-local indent-tabs-mode nil)
            (setq-local tab-width 4)
            (setq-local emacs-lisp-indent-offset 4)

            ;; Enable `electric-pair-mode` for auto-pairing parentheses
            (electric-pair-mode 1)

            ;; Highlight matching parentheses
            (show-paren-mode 1)
            ))

;; --- 2. `elisp-autofmt` (Auto-format ELisp) ---
(use-package elisp-autofmt
  :straight t
  :hook (emacs-lisp-mode . elisp-autofmt-mode) ; Enable the minor mode in elisp buffers
  :config
  ;; Automatically format Emacs Lisp buffers before saving
  (add-hook 'before-save-hook 'elisp-autofmt-buffer nil 'local)
  )

; (use-package ellsp
;   :straight t
;   :after lsp-mode ; Ensure `lsp-mode` is loaded first
;   :hook (emacs-lisp-mode . lsp-deferred) ; Activate LSP for `emacs-lisp-mode` buffers
;   :config
;   ;; You can add `ellsp` specific configurations here if needed.
;   ;; For example, enabling lsp-ui if you want a more visual LSP experience:
;   ;; (use-package lsp-ui :straight t :commands lsp-ui-mode)
;   ;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;   )

;; --- 4. `checkdoc` (Emacs Lisp Docstring Checker) ---
; (use-package checkdoc
;   :straight t
;   :hook (emacs-lisp-mode) ; Enable checkdoc-mode
;   :config
;   ;; Customize `checkdoc` behavior as needed
;   (setq checkdoc-ignore-regexp "^\\s-*\\(\\`\\|\\s-+\\)[[:alnum:]]*$")
;   )

;; --- 5. `eval-sexp-fu` (Optional: Enhanced S-expression Evaluation) ---
;; Provides more powerful evaluation commands.
(use-package eval-sexp-fu
  :straight t
  :after emacs-lisp-mode ; Ensure it loads after elisp mode
  :config
  (eval-sexp-fu-mode 1) ; Enable the minor mode for elisp buffers
  )

(provide 'init-elisp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-elisp.el ends here
