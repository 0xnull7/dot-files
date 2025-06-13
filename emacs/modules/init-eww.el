;;; init-eww.el --- Configuration for EWW (Emacs Web Wowser) -*- lexical-binding: t; -*-

;;; Commentary:
;; This file initializes EWW, Emacs's built-in web browser.
;; It includes configuration for EWW buffer naming and integration with external browsers.

;;; Code:

(use-package eww
  :straight nil
  :ensure nil
  :commands (eww)
  :hook (eww-mode .
              (lambda ()
                ;; Rename EWW's buffer to "eww" (or "eww<2>", etc.) for new pages.
                ;; This ensures each EWW page opens in a new buffer, rather than overwriting.
                (rename-buffer "eww" t)))
  :config
  ;; Conditional configuration: if EAF (Emacs Application Framework) is not ready,
  ;; set EWW as the default browser for `browse-url`.
  ;; Assuming `eaf-env-p` is defined in `init-const.el` and loaded before this.
  (unless (bound-and-true-p eaf-env-p) ; Use `bound-and-true-p` for robustness.
    (setq browse-url-browser-function 'eww-browse-url)
    ;; You might also want to set `eww-retrieve-command` for specific HTTP clients like `curl` or `wget`.
    ;; (setq eww-retrieve-command "curl -sL --compressed")
    )
  )

(provide 'init-eww)
;;; init-eww.el ends here
