;;; init-syntax.el --- Syntax and Spell Checking Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures `flycheck` for on-the-fly syntax checking and
;; `flyspell` for integrated spell checking across various modes in Emacs.

;;; Code:

;; --- 1. `flycheck` (On-the-fly Syntax Checking) ---
(use-package flycheck
  :straight t
  :defer t
  :diminish flycheck-mode

  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)

  :custom
  (flycheck-global-modes
   '(not outline-mode diff-mode shell-mode eshell-mode term-mode))
  ;; Inherit `emacs-lisp-mode` load path for Emacs Lisp checking.
  (flycheck-emacs-lisp-load-path 'inherit)
  ;; Set indication mode based on whether Emacs is graphical or terminal.
  (flycheck-indication-mode (if (display-graphic-p) 'right-fringe 'right-margin))

  :config
  ;; --- 1a. `flycheck-posframe` (Graphical Error Display) ---
  ;; Use `posframe` for non-intrusive error/warning messages in graphical Emacs.
  (if (display-graphic-p)
      (use-package flycheck-posframe
        :straight t ; Ensure `flycheck-posframe` is managed and installed.
        :hook (flycheck-mode . flycheck-posframe-mode) ; Activate in flycheck-mode.
        :custom
        ;; Custom faces for `flycheck-posframe` messages.
        (flycheck-posframe-face ((t (:foreground ,(face-foreground 'success)))))
        (flycheck-posframe-info-face ((t (:foreground ,(face-foreground 'success)))))
        ;; Position the popup window.
        (flycheck-posframe-position 'window-bottom-left-corner)
        ;; Border width for the popup.
        (flycheck-posframe-border-width 3)
        ;; Inhibit `posframe` display when `company-mode` is active to avoid conflicts.
        (flycheck-posframe-inhibit-functions
         '((lambda (&rest _) (bound-and-true-p company-backend))))
        )

    ;; --- 1b. `flycheck-pos-tip` (Terminal Error Display) ---
    ;; Fallback to `pos-tip` for error/warning messages in terminal Emacs.
    (use-package flycheck-pos-tip
      :straight t ; Ensure `flycheck-pos-tip` is managed and installed.
      :hook (flycheck-mode . flycheck-pos-tip-mode) ; Activate in flycheck-mode.
      :custom
      (flycheck-pos-tip-timeout 30) ; Timeout for `pos-tip` display in seconds.
      ))

  ;; --- 1c. `flycheck-popup-tip` (General Popup Display) ---
  ;; Another general popup utility, possibly for different contexts.
  (use-package flycheck-popup-tip
    :straight t ; Ensure `flycheck-popup-tip` is managed and installed.
    :hook (flycheck-mode . flycheck-popup-tip-mode) ; Activate in flycheck-mode.
    )

  ;; Define a custom fringe bitmap for Flycheck.
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  ;; --- 1d. `flycheck-vale` (Vale for Prose Linting) ---
  ;; Integrate Vale for linting prose (e.g., Markdown, LaTeX).
  (when (executable-find "vale")
    (use-package flycheck-vale
      :straight t ; Ensure `flycheck-vale` is managed and installed.
      :config
      (flycheck-vale-setup) ; Initialize Vale checker.
      ;; Add Vale checker to `latex-mode`. Add other text modes as needed.
      (flycheck-add-mode 'vale 'latex-mode)
      ;; (flycheck-add-mode 'vale 'markdown-mode)
      ;; (flycheck-add-mode 'vale 'org-mode)
      ))
  )

;; --- 2. `flyspell` (On-the-fly Spell Checking) ---
(use-package flyspell
  :straight nil
  :diminish flyspell-mode
  :if (executable-find "aspell")
  :hook
  (((text-mode prog-mode outline-mode latex-mode org-mode markdown-mode) . flyspell-mode))

  :custom
  (flyspell-issue-message-flag nil) ; Do not show messages about issues in the minibuffer.
  (ispell-program-name "aspell") ; Use `aspell` as the spell checker backend.
  ;; Extra arguments for `aspell` for better suggestions and camel case handling.
  (ispell-extra-args
   '("--sug-mode=ultra" "--lang=en_US" "--camel-case"))

  :config
  ;; --- 2a. `flyspell-correct-ivy` (Ivy Integration for Flyspell Corrections) ---
  (use-package flyspell-correct-ivy
    :straight t
    :after ivy
    :bind
    ;; Remap `flyspell-correct-word-before-point` to use `flyspell-correct-wrapper`
    ;; and bind `C-.` for quick corrections.
    (:map flyspell-mode-map
          ([remap flyspell-correct-word-before-point] . flyspell-correct-wrapper)
          ("C-." . flyspell-correct-wrapper))
    :custom
    (flyspell-correct-interface #'flyspell-correct-ivy) ; Set Ivy as the correction interface.
    ))

(provide 'init-syntax)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-syntax.el ends here
