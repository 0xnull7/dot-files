;;; init-word-wrap.el --- Smart Word Wrap Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file initializes intelligent word wrapping using `adaptive-wrap`,
;; `visual-line-mode`, and `visual-fill-column-mode`. It provides language-aware
;; indentation for wrapped lines and fine-grained control over wrapping behavior.

;;; Code:

;; --- 1. User-Configurable Variables ---
(defvar +word-wrap-extra-indent 'double
  "The amount of extra indentation for wrapped code lines.
When 'double, indent by twice the major-mode indentation.
When 'single, indent by the major-mode indentation.
When a positive integer, indent by this fixed amount.
When a negative integer, dedent by this fixed amount.
Otherwise no extra indentation will be used.")

(defvar +word-wrap-fill-style 'auto
  "How to handle `fill-column' in `+word-wrap-mode'.
When 'auto, long lines will soft-wrap at `fill-column'. If `auto-fill-mode' is
enabled, its behaviour will not be affected.
When 'soft, long lines will soft-wrap at `fill-column' and `auto-fill-mode' will
be forcibly disabled.
Otherwise long lines will soft-wrap at the window margin and `auto-fill-mode'
will not be affected.")

(defvar +word-wrap-disabled-modes
  '(fundamental-mode so-long-mode help-mode Info-mode doc-view-mode
    magit-log-mode magit-diff-mode dired-mode special-mode comint-mode
    term-mode shell-mode eshell-mode vterm-mode nov-mode)
  "Major modes where `+global-word-wrap-mode' should *not* enable
`+word-wrap-mode`. Add any modes where you prefer default or no wrapping.")

(defvar +word-wrap-visual-modes
  '(org-mode outline-mode)
  "Major modes where `+word-wrap-mode' should use `visual-line-mode'
instead of `adaptive-wrap-prefix-mode' for simple line wrapping.")

(defvar +word-wrap-text-modes
  '(text-mode markdown-mode markdown-view-mode gfm-mode gfm-view-mode
    rst-mode latex-mode LaTeX-mode conf-mode properties-mode bibtex-mode)
  "Major modes where `+word-wrap-mode' should *not* provide extra indentation.
These are typically text-based modes where uniform wrapping is desired.")

;; --- 2. Internal Variables (do not modify directly) ---
;; These are used internally by the word wrap system.
(defvar-local +word-wrap--major-mode-is-visual nil
  "Internal flag: t if current major mode is in `+word-wrap-visual-modes'.")
(defvar-local +word-wrap--major-mode-is-text nil
  "Internal flag: t if current major mode is in `+word-wrap-text-modes'.")
(defvar-local +word-wrap--enable-adaptive-wrap-mode nil
  "Internal flag: t if `adaptive-wrap-prefix-mode' should be enabled.")
(defvar-local +word-wrap--enable-visual-line-mode nil
  "Internal flag: t if `visual-line-mode' should be enabled.")
(defvar-local +word-wrap--enable-visual-fill-mode nil
  "Internal flag: t if `visual-fill-column-mode' should be enabled.")
(defvar-local +word-wrap--disable-auto-fill-mode nil
  "Internal flag: t if `auto-fill-mode' should be temporarily disabled.")
(defvar-local +word-wrap--major-mode-indent-var 'tab-width
  "Internal variable: The variable used for major mode indentation (e.g., `tab-width`).")

;; --- 3. Helper Functions for Context Detection ---
(defun +word-wrap--point-in-comment-p (&optional pt)
  "Return non-nil if point is in a comment.
PT defaults to the current position. Uses `syntax-ppss'."
  (let ((pt (or pt (point))))
    (save-excursion
      (nth 4 (syntax-ppss pt))))) ; 4th element of `syntax-ppss` result is `in-comment`.

(defun +word-wrap--point-in-string-p (&optional pt)
  "Return non-nil if point is inside a string.
PT defaults to the current position. Uses `syntax-ppss'."
  (let ((pt (or pt (point))))
    (save-excursion
      (nth 3 (syntax-ppss pt))))) ; 3rd element of `syntax-ppss` result is `in-string`.

(defun +word-wrap--point-in-string-or-comment-p (&optional pos)
  "Return non-nil if POS is in a string or comment."
  (or (+word-wrap--point-in-string-p pos)
      (+word-wrap--point-in-comment-p pos)))

;; --- 4. Indentation Calculation Function ---
(defun +word-wrap--calc-extra-indent (p)
  "Calculate extra word-wrap indentation at point.
This function is advised onto `adaptive-wrap-fill-context-prefix'."
  (if (not (or +word-wrap--major-mode-is-text
               (+word-wrap--point-in-string-or-comment-p p)))
      (pcase +word-wrap-extra-indent
        ('double
         (* 2 (symbol-value +word-wrap--major-mode-indent-var)))
        ('single
         (symbol-value +word-wrap--major-mode-indent-var))
        ((and (pred integerp) fixed)
         fixed)
        (_ 0))
    0))

;; --- 5. Advice for `adaptive-wrap-fill-context-prefix` ---
(defun +word-wrap--adjust-extra-indent-a (orig-fn &rest args)
  "Advice for `adaptive-wrap-fill-context-prefix' to add extra indent."
  (let ((orig (apply orig-fn args))
        (extra (+word-wrap--calc-extra-indent (car args))))
    (concat orig (make-string extra ?\ ))))

;; --- 6. `+word-wrap-mode` (Per-buffer Minor Mode) ---
;; This minor mode enables adaptive wrapping and related features in the current buffer.
(define-minor-mode +word-wrap-mode
  "Wrap long lines in the buffer with language-aware indentation.
This mode configures `adaptive-wrap', `visual-line-mode' and
`visual-fill-column-mode' to wrap long lines without modifying the buffer
content. This is useful when dealing with legacy code which contains
gratuitously long lines, or running emacs on your wrist-phone.

Wrapped lines will be indented to match the preceding line. In code buffers,
lines which are not inside a string or comment will have additional indentation
according to the configuration of `+word-wrap-extra-indent'.

Long lines will wrap at the window margin by default, or can optionally be
wrapped at `fill-column' by configuring `+word-wrap-fill-style'."
  :init-value nil
  :group 'word-wrap
  :global nil ; This is a buffer-local minor mode, globalized by `+global-word-wrap-mode`
  (if +word-wrap-mode
      ;; Enable `+word-wrap-mode`
      (progn
        (setq-local
         +word-wrap--major-mode-is-visual
         (memq major-mode +word-wrap-visual-modes)
         +word-wrap--major-mode-is-text
         (memq major-mode +word-wrap-text-modes)
         +word-wrap--enable-adaptive-wrap-mode
         (and (not (bound-and-true-p adaptive-wrap-prefix-mode))
              (not +word-wrap--major-mode-is-visual))
         +word-wrap--enable-visual-line-mode
         (not (bound-and-true-p visual-line-mode))
         +word-wrap--enable-visual-fill-mode
         (and (not (bound-and-true-p visual-fill-column-mode))
              (memq +word-wrap-fill-style '(auto soft)))
         +word-wrap--disable-auto-fill-mode
         (and (bound-and-true-p auto-fill-function)
              (eq +word-wrap-fill-style 'soft)))
        (unless +word-wrap--major-mode-is-visual
          ;; Try to determine the major mode's indentation variable
          (setq-local +word-wrap--major-mode-indent-var
                      (let ((indent-var (caddr (dtrt-indent--search-hook-mapping major-mode))))
                        (if (listp indent-var)
                            (car indent-var)
                          (or indent-var 'tab-width)))) ; Fallback to `tab-width`
          (advice-add #'adaptive-wrap-fill-context-prefix :around #'+word-wrap--adjust-extra-indent-a))
        (when +word-wrap--enable-adaptive-wrap-mode
          (adaptive-wrap-prefix-mode +1))
        (when +word-wrap--enable-visual-line-mode
          (visual-line-mode +1))
        (when +word-wrap--enable-visual-fill-mode
          (visual-fill-column-mode +1))
        (when +word-wrap--disable-auto-fill-mode
          (auto-fill-mode -1)))

    ;; Disable `+word-wrap-mode`
    (unless +word-wrap--major-mode-is-visual
      (advice-remove #'adaptive-wrap-fill-context-prefix #'+word-wrap--adjust-extra-indent-a))
    (when +word-wrap--enable-adaptive-wrap-mode
      (adaptive-wrap-prefix-mode -1))
    (when +word-wrap--enable-visual-line-mode
      (visual-line-mode -1))
    (when +word-wrap--enable-visual-fill-mode
      (visual-fill-column-mode -1))
    (when +word-wrap--disable-auto-fill-mode
      (auto-fill-mode +1))))

;; --- 7. `+global-word-wrap-mode` (Global Minor Mode) ---
;; This global minor mode activates `+word-wrap-mode` in most buffers.
(defun +word-wrap--enable-global-mode ()
  "Enable `+word-wrap-mode' for `+word-wrap-global-mode'.
Wrapping will be automatically enabled in all modes except special modes, or
modes explicitly listed in `+word-wrap-disabled-modes'."
  (unless (or (eq (get major-mode 'mode-class) 'special)
              (memq major-mode +word-wrap-disabled-modes))
    (+word-wrap-mode +1)))

(define-globalized-minor-mode +global-word-wrap-mode
  +word-wrap-mode
  +word-wrap--enable-global-mode
  :group 'word-wrap)

;; --- 8. Package Declarations and Hook Setup ---
;; Ensure necessary packages are installed via `straight.el`.
(use-package adaptive-wrap :straight t)
(use-package visual-fill-column :straight t)
(use-package dtrt-indent :straight t) ; Required for intelligent indentation lookup.

;; Set up initial hooks for word wrapping.
;; It's generally better to remove `visual-line-mode` from major-mode hooks
;; if `+word-wrap-mode` takes over wrapping, to prevent conflicts.
(eval-when-compile
  (when (memq 'visual-line-mode text-mode-hook)
    (remove-hook 'text-mode-hook #'visual-line-mode)))

;; Add `+word-wrap-mode` to text-mode-hook to activate it in text-based modes.
;; For a truly global enablement for code modes, `+global-word-wrap-mode` handles it.
(add-hook 'text-mode-hook #'+word-wrap-mode)

;; Enable the global word wrap mode after Emacs finishes initializing.
(add-hook 'after-init-hook #'+global-word-wrap-mode)

(provide 'init-word-wrap)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-word-wrap.el ends here
