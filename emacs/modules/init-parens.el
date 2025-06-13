;;; init-parens.el --- Parenthesis Management Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file sets up `smartparens` for smart parenthesis handling and
;; customizes `show-paren-mode` to display matching parentheses more effectively,
;; including those that are off-screen.

;;; Code:

;; --- 1. `smartparens` (Intelligent Parenthesis Handling) ---
(use-package smartparens
  :straight t
  :hook (prog-mode . smartparens-mode)
  :diminish smartparens-mode
  :bind
  (:map smartparens-mode-map
        ("C-M-f" . sp-forward-sexp)            ; Move forward over a S-expression.
        ("C-M-b" . sp-backward-sexp)           ; Move backward over a S-expression.
        ("C-M-a" . sp-backward-down-sexp)      ; Move to the beginning of the current S-expression.
        ("C-M-e" . sp-up-sexp)                 ; Move up one level in the S-expression tree.
        ("C-M-w" . sp-copy-sexp)               ; Copy the current S-expression.
        ("C-M-k" . sp-change-enclosing)        ; Change the type of enclosing parens (e.g., `()` to `[]`).
        ("M-k" . sp-kill-sexp)                 ; Kill the current S-expression.
        ("C-M-<backspace>" . sp-splice-sexp-killing-backward) ; Splice S-expression and kill backward.
        ("C-S-<backspace>" . sp-splice-sexp-killing-around)   ; Splice S-expression and kill around.
        ("C-]" . sp-select-next-thing-exchange)) ; Select next thing, exchange with previous.

  :custom
  (sp-escape-quotes-after-insert nil)

  :config
  ;; Disable auto-pairing of single quotes in `emacs-lisp-mode` to avoid issues with strings.
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  ;; Disable auto-pairing of square brackets in `org-mode` to prevent conflicts with links.
  (sp-local-pair 'org-mode "[" nil :actions nil)

  ;; Custom configuration for `smartparens` interactions.
  ;; You can add more `sp-local-pair` or `sp-with-modes` settings here.
  ;; Example: Configure `smartparens` to not pair comments in specific modes.
  ;; (sp-with-modes '(prog-mode)
  ;;   (sp-local-pair "//" nil :actions '(navigate)))
  )

;; --- 2. `show-paren-mode` (Visual Parenthesis Matching) ---
(show-paren-mode 1)

(remove-hook 'post-self-insert-hook #'blink-paren-post-self-insert-function)

(setq blink-matching-paren 'show)

;; Enhanced off-screen parenthesis matching display.
;; This `advice` provides a temporary overlay to display the matching parenthesis
;; when it's outside the current visible window.
(defvar-local show-paren--off-screen-overlay nil
  "Overlay used to display off-screen matching parenthesis.")

(defun display-line-overlay+ (pos text)
  "Create an overlay to display TEXT at POS."
  (let ((ov (make-overlay pos pos)))
    (overlay-put ov 'display-type 'text)
    (overlay-put ov 'after-string (concat " " text))
    (overlay-put ov 'face 'show-paren-match-face)
    (overlay-put ov 'priority -1) ; Lower priority so it doesn't obscure other overlays.
    (overlay-put ov 'keymap (make-sparse-keymap))
    (overlay-put ov 'local-map t)
    ov))

(advice-add #'show-paren-function :after
            (defun show-paren--off-screen-match-display (&rest _args)
              "Display matching line for off-screen paren with a temporary overlay."
              ;; Delete existing overlay before creating a new one.
              (when (overlayp show-paren--off-screen-overlay)
                (delete-overlay show-paren--off-screen-overlay))

              ;; Check if it's appropriate to show match info,
              ;; similar to `blink-paren-post-self-insert-function` logic.
              (when (and (overlay-buffer show-paren--overlay) ; Is there a match highlighted?
                         (not (or cursor-in-echo-area
                                  executing-kbd-macro
                                  noninteractive
                                  (minibufferp)
                                  this-command))
                         (and (not (bobp))
                              (memq (char-syntax (char-before)) '(?\) ?\$)))
                         ;; Check if the matching paren is actually off-screen.
                         (let* ((match-start (overlay-start show-paren--overlay))
                                (window-top (window-start))
                                (window-bottom (window-end)))
                           (or (< match-start window-top) (> match-start window-bottom))))
                ;; Rebind `minibuffer-message` locally to capture the message
                ;; and display it as an overlay instead.
                (cl-letf (((symbol-function #'minibuffer-message)
                           (lambda (msg &rest args)
                             (let ((msg (apply #'format-message msg args)))
                               (setq show-paren--off-screen-overlay
                                     (display-line-overlay+
                                      (overlay-start show-paren--overlay) msg))))))
                  (blink-matching-open))))) ; Call `blink-matching-open` to generate the message.

(provide 'init-parens)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-parens.el ends here
