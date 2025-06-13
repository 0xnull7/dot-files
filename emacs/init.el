;;; init.el --- Primary Emacs initialization file -*- lexical-binding: t; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is the main init.el file for My Emacs Config. It bootstraps the
;; package manager, sets up core behavior, loads the modular configuration,
;; and handles early theme loading.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;; Clear any lingering custom settings from `customize-ui` and prevent future saves.
;; This should be at the very top of your init.el.
; (setq custom-file nil)
; (custom-set-variables nil) ; Clear variables set by Custom
; (custom-set-faces nil)     ; Clear faces set by Custom

;;;; --- General UI & Behavior Settings ---
;; These are fundamental UI settings. Theme-specific UI settings will be in init-ui-config.el.

;; Enable column number mode in the mode line (shows current column).
(column-number-mode 1)

;; Show a fill-column indicator (a vertical line at 'fill-column').
(display-fill-column-indicator-mode 1)
;; Set the column for the indicator and auto-wrapping.
(setq fill-column 150)

;; Enable electric-pair-mode globally for auto-pairing delimiters like (), [], {}.
; (global-electric-pair-mode 1)
;; Customize which pairs are automatically inserted/matched.
; (setq electric-pair-pairs
;       '((34 . 34) (8216 . 8217) (8220 . 8221) (40 . 41) (91 . 93) (123 . 125) (39 . 39) (60 . 62)))

;; Enable global line numbers in all buffers.
(global-display-line-numbers-mode 1)

;; Control justification of text (e.g., in text-mode).
(setq default-justification 'full)

;; Control indentation of individual lines in `fill-paragraph`.
(setq fill-individual-varying-indent t)

;; Show buffer size in the mode line.
(size-indication-mode 1)

;; Adjust visual-fill-column when text-scale-mode is active.
(setq visual-fill-column-adjust-for-text-scale t)

;; Disable various UI elements (common for a minimal setup).
;; These should be consistent with your theme/modeline.
(menu-bar-mode -1)   ; Hide the menu bar
(scroll-bar-mode -1) ; Hide scroll bars
(tool-bar-mode -1)   ; Hide the tool bar
(tooltip-mode -1)    ; Disable tooltips

;; --- Garbage Collection Management ---
;; Aggressively defer GC during startup, restored later for performance.
;; If 'my--original-gc-cons-percentage' and 'my--original-file-name-handler-alist'
;; are truly early settings, they are best defined in early-init.el.
;; The hook itself is correct to be here.
(defun my-gc-finalize-startup ()
  "Restore GC to runtime values and re-enable deferred handlers."
  ;; The variables my--original-gc-cons-percentage and my--original-file-name-handler-alist
  ;; are set in early-init.el (or should be).
  (setq gc-cons-threshold (* 64 1024 1024) ; 64MB
        gc-cons-percentage my--original-gc-cons-percentage ; Use value from early-init.el
        file-name-handler-alist my--original-file-name-handler-alist)

  ;; For a fully automated and robust solution, consider replacing this
  ;; manual GC tuning with the `gcmh` package in the future.
  (message "GC settings restored for optimal runtime performance."))

;; This hook runs once after the frame is visible and init is complete.
(add-hook 'emacs-startup-hook #'my-gc-finalize-startup)

;; --- Load Custom Configuration Modules ---
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(message "Requiring custom modules...")

;; Foundational settings first
(require 'init-global-config) ; General Emacs settings not specific to a mode
(require 'init-const)         ; Constants or global variables if you have them

;; UI and Display - THEME WILL BE LOADED HERE
(require 'init-ui-config)     ; Your theme, dashboard, and general UI adjustments
(require 'init-buffer)        ; Buffer management
(require 'init-scroll)        ; Scrolling behavior
(require 'init-winner)        ; Window state (winner-mode)

;; Core Editing, Completion, and Navigation
(require 'init-editing)       ; General text editing enhancements
(require 'init-completion)    ; All completion frameworks (Vertico, Consult, Company, etc.)
(require 'init-search)        ; Search enhancements
(require 'init-navigation)    ; Navigation utilities (avy, dumb-jump)
(require 'init-comment)       ; Commenting utilities
(require 'init-parens)        ; Parenthesis matching/management
(require 'init-undo-tree)     ; Undo history
(require 'init-word-wrap)     ; Word wrapping
(require 'init-indent)        ; Indentation helpers

;; File Management & Project Tools
(require 'init-dired)         ; Dired enhancements
(require 'init-projects)      ; Project management
(require 'init-buildsystem)   ; Build system integration (e.g., compile commands)
(require 'init-magit)         ; Git integration

;; LSP and Debugging (should come before specific languages)
(require 'init-lsp)           ; General LSP setup
(require 'init-debbugs)       ; Debugging tools

;; Language Modes (order generally doesn't matter much here, but can group similar)
(require 'init-ada)
(require 'init-assembly)
(require 'init-bash)
(require 'init-cc)
(require 'init-clojure)
(require 'init-cmake)
(require 'init-csharp-dotnet)
(require 'init-d)
(require 'init-elisp)
(require 'init-gdscript)
(require 'init-go)
(require 'init-haskell)
(require 'init-java)
(require 'init-kotlin)
(require 'init-latex)
(require 'init-lua)
(require 'init-markdown)
(require 'init-nim)
(require 'init-pascal)
(require 'init-pdf)
(require 'init-perl)
(require 'init-php)
(require 'init-powershell)
(require 'init-python)
(require 'init-racket)
(require 'init-ruby)
(require 'init-rust)
(require 'init-shell)
(require 'init-sql)
(require 'init-syntax)
(require 'init-vlang)
(require 'init-webdev)
(require 'init-zig)

;; Other Utilities & Fun Stuff
(require 'init-misc)          ; General utilities not tied to a specific area
(require 'init-discover-my-major) ; Discovering modes
(require 'init-erc)           ; IRC client
(require 'init-eww)           ; Emacs Web Wowser
(require 'init-func)          ; Custom functions
(require 'init-games)         ; Emacs games!
(require 'init-tramp)         ; Remote editing
(require 'init-zone)          ; Idle display mode

(message "All custom modules required.")

;; --- Finalization ---
(message "Emacs initialization complete. Welcome back %s!" user-login-name)

(provide 'init)

;;; init.el ends here
