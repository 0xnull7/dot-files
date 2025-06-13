;;; init-misc.el --- Miscellaneous Utility Packages Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains configurations for smaller, general-purpose utility packages,
;; enhancing various aspects of the Emacs environment.

;;; Code:

;; --- 1. `quickrun` (Run Commands Quickly) ---
(use-package quickrun
  :straight t
  :commands (quickrun)
  :config
  ;; Example: Bind `quickrun` to a convenient key.
  (global-set-key (kbd "C-c r") #'quickrun)
  )

;; --- 2. `siege-mode` (Visual Effects) ---
; (use-package siege-mode
;   :straight (:host github :repo "tslilc/siege-mode") ; Explicitly define host and repo.
;   :commands (siege-mode)
;   :init
;   (siege-mode 1)
;   )

;; --- 3. `auto-package-update` (Automatic Package Management) ---
; (use-package auto-package-update
;   :straight t
;   :if (not (daemonp))
;   :custom
;   (auto-package-update-interval 7)           ; Check for updates every 7 days.
;   (auto-package-update-prompt-before-update t) ; Ask for confirmation before updating.
;   (auto-package-update-delete-old-versions t) ; Automatically delete old package versions.
;   (auto-package-update-hide-results t)       ; Hide the update results buffer.
;   :config
;   ;; Initiate the update check when this configuration loads.
;   (auto-package-update-maybe))

;; --- 4. `fireplace` (Visual Effects) ---
(use-package fireplace
  :straight t
  :commands (fireplace)
  :config
  ;; Consider binding these commands if you use them frequently.
  ;; (global-set-key (kbd "C-c f u") #'fireplace-up)
  ;; (global-set-key (kbd "C-c f d") #'fireplace-down)
  ;; (global-set-key (kbd "C-c f t") #'fireplace-toggle-smoke)
  ;; (global-set-key (kbd "C-c f s") #'fireplace-toggle-sound) ; Requires `ffplay`.
  ;; (global-set-key (kbd "C-c f q") #'fireplace-off)
  )


;; --- 6. `mode-icons` (Mode Line Icons) ---
(use-package mode-icons
  :straight t
  :init
  (mode-icons-mode)
  )

;; --- 7. `parrot` (Animated Parrot in Mode Line) ---
(use-package parrot
  :straight t
  :init
  (parrot-mode)
  )

;; --- 8. `global-prettify-symbols-mode` (Unicode Symbols) ---
(global-prettify-symbols-mode +1)

;; --- 9. `rainbow-delimiters` (Colorized Parentheses) ---
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode)
  )

;; --- 10. `rainbow-mode` (Colorized Text Based on Hash) ---
(use-package rainbow-mode
  :straight t
  :init
  (rainbow-mode t)
  )

;; --- 11. `restart-emacs` (Emacs Restart Command) ---
(use-package restart-emacs
  :straight t
  :commands (restart-emacs) ; Correct usage of :commands
  :bind ("C-x C-r" . restart-emacs) ; Corrected: use :bind for keybindings
  )

;; (use-package artist-mode
;;   :straight t
;;   :commands (artist-mode))

;; (use-package cheatsheet
;;   :straight t
;;   :commands (cheatsheet-map))

;; (use-package dired-k
;;   :straight t
;;   :after dired)

;; (use-package vertico
;;   :straight t
;;   :custom
;;   ;; (vertico-scroll-margin 0)
;;   ;; (vertico-count 20)
;;   ;; (vertico-resize t)
;;   ;; (vertico-cycle t)
;;   :init
;;   (vertico-mode t))

;; (use-package gnus
;;   :straight t) ; For News/Email client

;; (use-package guess-language
;;   :straight t)

;; (use-package messages
;;   :straight t) ; Typically built-in, but can be managed by use-package if needed.

;; (use-package nyan-mode
;;   :straight t
;;   :init (nyan-mode 1))

(provide 'init-misc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-misc.el ends here
