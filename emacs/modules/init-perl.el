;;; init-perl.el --- Perl Language Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Emacs for Perl development, leveraging the built-in
;; `perl-mode` or the more advanced `cperl-mode`, and integrating with `lsp-mode`
;; for `perl-language-server`.

;;; Code:

;; --- 1. `cperl-mode` (Advanced Perl Mode) ---
(use-package cperl-mode
  :straight t
  :mode
  (("\\.pl\\'" . cperl-mode)   ; Perl scripts
   ("\\.pm\\'" . cperl-mode)   ; Perl modules
   ("\\.t\\'" . cperl-mode))   ; Perl tests
  :config
  (setq cperl-indent-level 4)
  (setq cperl-continued-statement-offset 4)
  (setq cperl-close-paren-offset 0)
  (setq cperl-auto-newline nil)
  (setq cperl-electric-parens nil)
  )

(provide 'init-perl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-perl.el ends here
