;;; init-latex.el --- Configuration for LaTeX and TeX Editing -*- lexical-binding: t; -*-

;;; Commentary:
;; This file sets up a robust LaTeX and TeX environment using AUCTeX,
;; `lsp-latex` for Language Server Protocol features, `cdlatex` for fast
;; LaTeX input, and `bibtex-mode` with LSP for bibliography management.

;;; Code:

;; --- 1. AUCTeX (Comprehensive TeX/LaTeX Environment) ---
(use-package auctex
  :straight t
  :mode (("\\.tex\\'" . latex-mode)
         ("\\.sty\\'" . tex-mode)
         ("\\.cls\\'" . tex-mode)
         ("\\.dtx\\'" . latex-mode))
  :custom
  ;; AUCTeX parses documents for completion and syntax highlighting.
  (TeX-parse-self t)
  ;; Automatically save style information for faster parsing next time.
  (TeX-auto-save t)
  ;; For large projects, ask for the master file. Set to `nil` to always ask.
  (TeX-master nil)
  ;; Use a modern TeX engine (LuaLaTeX is generally recommended).
  (TeX-engine 'luatex)
  ;; Ensure PDF mode is active for PDF output. (Often redundant in newer AUCTeX).
  (TeX-PDF-mode t)
  ;; Default command for compilation.
  (TeX-command-default "LuaLaTeX")
  ;; Indentation settings for LaTeX environments.
  (LaTeX-indent-level 4)
  ; (LaTeX-item-indent 0)
  (setq LaTeX-document-regexp nil)
  (setq LaTeX-indent-environment-list '(("document" current-indentation)))
  (TeX-brace-indent-level 4)
  (TeX-newline-function 'newline-and-indent)
  ;; SyncTeX settings for forward/inverse search with PDF viewer.
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-start-server t)
  ;; Use pdf-tools for viewing and synchronization.
  (TeX-view-program-selection '((output-pdf "pdf-tools")))
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  ;; Revert document buffer after successful compilation.
  (TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  ;; Don't ask for confirmation when cleaning build artifacts.
  (TeX-clean-confirm nil)
  ;; Automatically insert braces after sub- and superscripts in math mode.
  (TeX-electric-sub-and-superscript t)
  ;; Smart math environment insertion with electric behavior.
  (TeX-electric-math '("\\(" "\\)"))
  ;; Don't insert magic quotes right away.
  (TeX-quote-after-quote t)
  (setq LaTeX-indent-environment-list
      '(("verbatim" current-indentation)
          ("lstlisting" current-indentation)
          ("minted" current-indentation)
          ("notebox" current-indentation)
          ("importantbox" current-indentation)
          ("definition" current-indentation)
          ("theorem" current-indentation)
          ("lemma" current-indentation)
          ("remark" current-indentation)
          ("abstract" current-indentation)
          ("tableofcontents" current-indentation)
          ("itemize" current-indentation)
          ("enumerate" current-indentation)
          ("description" current-indentation)
          ("document" current-indentation)
          ))
  :hook
  ;; Hooks to run when `LaTeX-mode` is activated.
  (LaTeX-mode . (lambda ()
                  (turn-on-reftex)      ; Enable RefTeX for cross-referencing.
                  (setq reftex-plug-into-AUCTeX t) ; Integrate RefTeX with AUCTeX.
                  (reftex-isearch-minor-mode) ; Enhance Isearch with RefTeX.
                  (flyspell-mode)       ; Enable spell checking.
                  (LaTeX-math-mode)     ; Enable easy math input (e.g., C-c C-e `).
                  (turn-on-auto-fill)   ; Enable auto-filling paragraphs.
                  (visual-line-mode) ; Uncomment if you prefer visual line wrapping.
                  ))

  :config
  ;; Fallback for older Emacs versions if `display-line-numbers-mode` is used.
  (when (version< emacs-version "26")
    (add-hook 'LaTeX-mode-hook #'display-line-numbers-mode))

  ;; Example of how to modify a TeX command (e.g., use `chktex`).
  ;; (setcdr (assoc "Check" TeX-command-list) "chktex -v6 %s")
  )

;; --- 2. CDLaTeX (Fast LaTeX Insertion) ---
(use-package cdlatex
  :straight t
  :hook (LaTeX-mode . turn-on-cdlatex)
  :custom
  ;; Customize math symbol auto-completion, e.g., for `^`.
  (cdlatex-math-symbol-alist '((?^ ("^{}" "\\hat{}"))))
  ;; Customize math modifications, e.g., for `\text`.
  (cdlatex-math-modify-alist '((116 "\\text" nil t)))
  )

;; --- 3. BibTeX (Bibliography Management) ---
(use-package bibtex
  :straight t        ; Ensure `bibtex` is managed by straight.el.
  :mode "\\.bib\\'" ; Activate `bibtex-mode` for .bib files.
  :hook ((bibtex-mode . lsp-deferred) ; Enable LSP for BibTeX files.
         (bibtex-mode . my/bibtex-fill-column)) ; Apply custom fill-column.
  :preface
  (defun my/bibtex-fill-column ()
    "Set `fill-column` to 120 characters for BibTeX entries."
    (setq-local fill-column 120)) ; Use `setq-local` to apply only to current buffer.
  :config
  ;; It's often useful to run `prog-mode-hook` for general programming mode features.
  (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
  ;; Use a modern BibTeX dialect.
  (bibtex-set-dialect 'biblatex)
  )

;; --- 4. LSP LaTeX (Language Server Protocol for LaTeX) ---
(use-package lsp-latex
  :straight t        ; Ensure `lsp-latex` is managed by straight.el.
  :hook (LaTeX-mode . (lambda ()
                         (require 'lsp-latex) ; Explicitly require `lsp-latex` before `lsp-deferred`.
                         (lsp-deferred)))     ; Enable LSP for LaTeX.
  :if (executable-find "texlab") ; Only load if `texlab` language server is found.
  :custom
  (lsp-latex-build-on-save t) ; Trigger a LaTeX build when saving the file.
  )

;; --- 5. Company-AUCTeX (Completion Backend for AUCTeX) ---
(use-package company-auctex
  :straight t      ; Ensure `company-auctex` is managed by straight.el.
  :after (company auctex) ; Load after `company` and `auctex`.
  :config
  (company-auctex-init)) ; Initialize the backend.

;; --- 6. Company-Math (Math Symbol Completion) ---
(use-package company-math
  :straight t      ; Ensure `company-math` is managed by straight.el.
  :after company   ; Load after `company`.
  :config
  (add-to-list 'company-backends 'company-math-symbols-latex)) ; Add as a Company backend.

(provide 'init-latex)
;;; init-latex.el ends here
