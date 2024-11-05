;; init-misc.el --- Miscellaneous Packages setup --- -*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commentary:
;; This file have some miscellaneous packages that make the user experience
;; more fun and elegant.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Code:

(eval-when-compile
  (require 'use-package))

(use-package fireplace
  :ensure t
  ;; C-+ Move fire up
  ;; C-- Move fire down
  ;; C-* Toggle smoke
  ;; C-= Toggle sound (requires ffplay)
  ;; q Turn off fire
  )

;; (use-package fsc.el)

;; (use-package gnus)

(use-package grandshell-theme)

;; (use-package guess-language)

;; (use-package messages)

(use-package mode-icons
  :ensure t
  :config
  (mode-icons-mode))

(use-package monokai-theme)

;; (use-package nyan-mode)

(use-package parrot
  :ensure t
  :config
  (parrot-mode))

(global-prettify-symbols-mode +1)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :config
  (rainbow-mode t))

(use-package restart-emacs)


(provide 'init-misc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-misc.el ends here




