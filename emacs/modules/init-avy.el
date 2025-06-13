;;; init-avy.el --- Configuration for avy, for rapid navigation -*- lexical-binding: t; -*-

;;; Commentary:
;; Avy provides commands to jump to visible text in a buffer using a few key presses.

;;; Code:

(use-package avy
  :straight t
  :defer t ; Explicit deferral is good practice.
  :bind (("C-c a c" . avy-goto-char-timer) ; Changed prefix from C-z
         ("C-c a l" . avy-goto-line)     ; Changed prefix from C-z
         ;; Add more avy commands as needed. Examples:
         ;; ("C-c a w" . avy-goto-word-or-subword)
         ;; ("C-c a j" . avy-goto-line-below)
         ;; ("C-c a k" . avy-goto-line-above)
         )
  :config
  ;; General Avy settings.
  (setq avy-timeout-seconds 0.3
        avy-style 'pre)

  ;; Face customization for Avy leads.
  (set-face-attribute 'avy-lead-face nil
                      :background "#51afef"
                      :foreground "#870000"
                      :weight 'bold)
  )

(provide 'init-avy)
;;; init-avy.el ends here
