;;; init-assembly.el --- Assembly Language Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Emacs for assembly language development.
;; It primarily uses `asm-mode` for general assembly syntax highlighting.
;; Note: Specific assemblers (NASM, MASM, GAS) are external tools, and Emacs
;; modes primarily provide syntax highlighting and basic indentation.
;; No separate modes are typically required for Linux vs. Windows assembly,
;; as `asm-mode` is quite generic for the syntax itself.

;;; Code:

;; --- 1. `asm-mode` (Major Mode for Assembly) ---
(use-package asm-mode
  :straight t
  :mode
  (("\\.s\\'" . asm-mode)    ; Generic assembly (e.g., GAS)
   ("\\.asm\\'" . asm-mode)  ; Common for NASM/MASM
   ("\\.inc\\'" . asm-mode)) ; Include files
  :config
  ;; Basic indentation for assembly files.
  (setq asm-indent-width 4)
  (setq asm-comment-column 40) ; Align comments at column 40
  ;; Adjust if using NASM: (setq asm-comment-start ";")
  ;; Adjust if using MASM: (setq asm-comment-start ";")
  ;; Adjust if using GAS: (setq asm-comment-start "#")

  ;; Optional: Customize syntax highlighting if `asm-mode` doesn't cover all directives
  ;; (font-lock-add-keywords 'asm-mode
  ;;                         '(("^[ \t]*\\(section\\|global\\|extern\\)\\b" 1 font-lock-preprocessor-face t)))

  ;; No common LSP server for generic assembly language that provides rich features
  ;; like completion or diagnostics in the same way as high-level languages.
  ;; You might use `M-x compile` to run your assembler (e.g., `nasm -f elf64 -o hello.o hello.asm`).
  )

(provide 'init-assembly)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-assembly.el ends here
