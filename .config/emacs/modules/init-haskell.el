;;; init-haskell.el --- -*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes haskell-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'use-package))

;; HaskellModePac
(use-package haskell-mode
  :mode "\\.hs\\'")

;; Install and configure lsp-haskell
(use-package lsp-haskell
  :hook ((haskell-mode haskell-literate-mode) . lsp-deferred)
  :config (setq lsp-haskell-server-path "haskell-language-server"))

;; -HaskellModePac

(provide 'init-haskell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-haskell.el ends here
