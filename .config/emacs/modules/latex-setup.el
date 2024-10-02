;;; latex-setup.el --- LaTeX writing environment setup

;;; Commentary:
;; This file sets up a LaTeX writing environment in Emacs with LSP,
;; auto-completion, snippets, and other helpful tools.

;;; Code:

;; Ensure use-package is available
(eval-when-compile
  (require 'use-package))

;; AUCTeX - enhanced LaTeX mode
(use-package auctex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-engine 'luatex)
  (setq TeX-command-default "LuaLaTeX")
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t))

;; CDLaTeX - fast LaTeX insertion
(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . turn-on-cdlatex)
  :config
  (setq cdlatex-math-symbol-alist '((?^ ("^{}" "\\hat{}"))))
  (setq cdlatex-math-modify-alist '((116 "\\text" nil t))))

;; Company-AUCTeX backend
(use-package company-auctex
  :ensure t
  :after (company auctex)
  :config
  (company-auctex-init))

;; Company-Math backend
(use-package company-math
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-math-symbols-latex))

;; LaTeX-specific Flycheck setup
;;(use-package flycheck-latex
;;  :ensure t
;;  :after flycheck)

(provide 'latex-setup)
;;; latex-setup.el ends here
