;; init-latex.el --- -*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes AUCTex
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-global-config)
  (require 'init-func)
  (require 'use-package))

;; (use-package tex                        ; TeX editing/processing
;;   :ensure auctex
;;   :straight (:type built-in)
;;   :defer t
;;   :config
;;   (setq TeX-parse-self t                ; Parse documents to provide completion
;;                                         ; for packages, etc.
;;         TeX-auto-save t                 ; Automatically save style information
;;         TeX-electric-sub-and-superscript t ; Automatically insert braces after
;;                                         ; sub- and superscripts in math mode
;;         TeX-electric-math '("\\(" "\\)")
;;         ;; Don't insert magic quotes right away.
;;         TeX-quote-after-quote t
;;         ;; Don't ask for confirmation when cleaning
;;         TeX-clean-confirm nil
;;         ;; Provide forward and inverse search with SyncTeX
;;         TeX-source-correlate-mode t
;;         TeX-source-correlate-method 'synctex)
;;   (setq-default TeX-master nil          ; Ask for the master file
;;                 TeX-engine 'luatex      ; Use a modern engine
;;                 ;; Redundant in 11.88, but keep for older AUCTeX
;;                 TeX-PDF-mode t)

;;   ;; Move to chktex
;;   (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s"))

;; (use-package tex-mode                   ; TeX mode
;;   :ensure auctex
;;   :straight (:type built-in)
;;   :defer t
;;   :config
;;   (font-lock-add-keywords 'latex-mode
;;                           `((,(rx "\\"
;;                                   symbol-start
;;                                   "fx" (1+ (or (syntax word) (syntax symbol)))
;;                                   symbol-end)
;;                              . font-lock-warning-face))))

;; (use-package latex                      ; LaTeX editing
;;   :ensure auctex
;;   :straight (:type built-in)
;;   :defer t
;;   :config
;;   ;; Teach TeX folding about KOMA script sections
;;   (setq TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
;;                             (,(rx (0+ space) "\\subsection*{") 3)
;;                             (,(rx (0+ space) "\\subsubsection*{") 4)
;;                             (,(rx (0+ space) "\\minisec{") 5))
;;         ;; No language-specific hyphens please
;;         LaTeX-babel-hyphen nil)

;;   (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode))    ; Easy math input


;; AUCTeXPac
(use-package auctex
  :ensure t
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil)
  (setq TeX-engine 'luatex)
  (setq TeX-command-default "LuaLaTeX")
  ;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  ;; to use pdfview with auctex
  (TeX-view-program-selection '((output-pdf "pdf-tools"))
                              TeX-source-correlate-start-server t)
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  (TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :hook
  (LaTeX-mode . (lambda ()
                  (turn-on-reftex)
                  (setq reftex-plug-into-AUCTeX t)
                  (reftex-isearch-minor-mode)
                  (setq TeX-PDF-mode t)
                  (setq TeX-source-correlate-method 'synctex)
                  (setq TeX-source-correlate-start-server t)))
  :config
  (when (version< emacs-version "26")
    (add-hook LaTeX-mode-hook #'display-line-numbers-mode)))
;; -AUCTeXPac

;; CDLaTeX - fast LaTeX insertion
(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . turn-on-cdlatex)
  :config
  (setq cdlatex-math-symbol-alist '((?^ ("^{}" "\\hat{}"))))
  (setq cdlatex-math-modify-alist '((116 "\\text" nil t))))

(use-package bibtex                     ; BibTeX editing
  :defer t
  :preface
  (defun my/bibtex-fill-column ()
    "Ensure that each entry does not exceed 120 characters."
    (setq fill-column 120))
  :hook ((bibtex-mode . lsp-deferred)
         (bibtex-mode . my/bibtex-fill-column))
  :config
  ;; Run prog mode hooks for bibtex
  (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

  ;; Use a modern BibTeX dialect
  (bibtex-set-dialect 'biblatex))

(use-package lsp-latex
  :if (executable-find "texlab")
  ;; To properly load `lsp-latex', the `require' instruction is important.
  :hook (LaTeX-mode . (lambda ()
                        (require 'lsp-latex)
                        (lsp-deferred)))
  :custom (lsp-latex-build-on-save t))

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

(provide 'init-latex)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-latex.el ends here
