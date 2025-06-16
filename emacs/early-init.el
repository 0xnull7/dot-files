;;; early-init.el --- Early startup code for maximum performance -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains code executed at the earliest possible stage of Emacs startup.
;; Its purpose is to configure settings that affect the package system, garbage
;; collection, and initial frame appearance BEFORE the main init files are loaded.
;; This leads to a significantly faster and smoother startup experience.

;;; Code:

;; Set `user-emacs-directory` to be XDG-compliant *before* package managers initialize.
;; This MUST be the first thing, or among the very first things, in this file.
(setq user-emacs-directory (expand-file-name "~/.config/emacs/"))

;; Ensure lexical-binding is set from the absolute start.
(setq lexical-binding t)

;; --- 1. Package System Configuration ---
;; Disable package.el as we exclusively use straight.el for package management.
;; This prevents conflicts and redundant work at startup.
(setq package-enable-at-startup nil)

;; Bootstrap straight.el and use-package very early.
;; This block ensures 'straight.el' is loaded from its installed location
;; and then makes 'use-package' available for your configuration.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install and configure use-package immediately after straight.el is bootstrapped.
;; This ensures 'use-package' is managed by straight.el and fully functional.
(straight-use-package 'use-package)
(setq use-package-always-ensure t   ; Always ensure packages are installed
      use-package-always-defer t    ; Always defer loading packages by default
      use-package-verbose t)        ; Show messages during use-package operations

;; Crucial: Ensure `bind-key` is loaded and its macros are available
;; for `use-package`'s advanced :bind forms (like :map).
;; This should also be managed by straight.el.
(straight-use-package 'bind-key)


;; --- 2. Performance Optimization: Garbage Collection & Handlers ---
;; Aggressively defer garbage collection during startup to minimize pauses.
;; These values will be reset to more reasonable defaults after startup is complete
;; by the `my-gc-finalize-startup` function in init.el.
(setq gc-cons-threshold (* 256 1024 1024)) ;; Increased to 256MB for more aggressive deferral
(setq gc-cons-percentage 0.9)              ;; Increased for more aggressive deferral

;; Store original values for restoration in init.el.
;; These variables are global and will be correctly restored via init.el's hook.
(defvar my--original-gc-cons-percentage gc-cons-percentage)
(defvar my--original-file-name-handler-alist file-name-handler-alist)

;; Deferring `file-name-handler-alist` is a major performance boost.
(setq file-name-handler-alist nil)

;; --- 3. Initial Frame and UI Configuration ---
;; These settings prevent UI flicker and ensure consistent frame sizing early.
(setq frame-resize-pixelwise nil)
(setq inhibit-default-frame-init t)
(setq x-gtk-resize-child-frames nil)

;; Use `default-frame-alist` for all initial frame settings.
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil)) ; Explicitly nil to disable

;; Avoid loading site-wide configuration for a more predictable and controlled setup.
(setq site-run-file nil)
(setq inhibit-startup-screen t)

;; Suppress the welcome message more reliably.
(setq inhibit-startup-echo-area-message t)

(provide 'early-init)
;;; early-init.el ends here
