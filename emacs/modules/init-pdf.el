;;; init-pdf.el --- PDF Viewing with PDF Tools -*- lexical-binding: t; -*-

;;; Commentary:
;; This file initializes and configures `pdf-tools` for efficient PDF viewing
;; within Emacs, including integration with TeX document compilation and display.

;;; Code:

;; --- 1. `pdf-tools` (PDF Viewer) ---
(use-package pdf-tools
  :straight t
  ;; Only load `pdf-tools` if:
  ;; 1. Emacs is running in a graphical display (not terminal).
  ;; 2. It's not on Windows (`*sys/win32*` is likely a custom variable).
  ;; 3. EAF (Emacs Application Framework) is not active (assuming `eaf-env-p` is a custom variable).
  :if (and (display-graphic-p) (not (bound-and-true-p *sys/win32*)) (not (bound-and-true-p eaf-env-p)))
  :mode "\\.pdf\\'" ; Automatically activate `pdf-view-mode` for `.pdf` files.
  :commands (pdf-loader-install pdf-view-mode)

  :custom
  ;; Configure AucTeX/TeX-mode to use `pdf-tools` for viewing PDF output.
  (TeX-view-program-selection '((output-pdf "pdf-tools")))
  (TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
  ;; Default display size for PDFs. `fit-page` (default) or `fit-width` are common.
  (pdf-view-display-size 'fit-page)

  :hook
  ;; Custom settings applied when `pdf-view-mode` is activated.
  (pdf-view-mode . (lambda ()
                     ;; Disable line numbers in PDF view mode for a cleaner look.
                     (display-line-numbers-mode -1)
                     ;; Add other PDF-specific settings here, e.g.:
                     ;; (visual-line-mode -1) ; Ensure word wrap is off
                     ;; (hl-line-mode -1) ; Disable line highlighting
                     ))

  :config
  ;; Crucial step: Install the `pdf-tools` server.
  ;; This compiles the necessary backend for PDF rendering.
  ;; This should be done only once. If you rebuild Emacs or update `pdf-tools`
  ;; significantly, you might need to run `M-x pdf-loader-install` manually.
  (pdf-loader-install)

  ;; It's often helpful to set `pdf-info-persist-file` to keep PDF scroll positions.
  ;; (setq pdf-info-persist-file (expand-file-name "~/.emacs.d/etc/pdf-cache/pdf-info.el"))
  ;; Ensure the directory exists:
  ;; (unless (file-directory-p (file-name-directory pdf-info-persist-file))
  ;;   (make-directory (file-name-directory pdf-info-persist-file) t))
  )

(provide 'init-pdf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-pdf.el ends here
