;;; init-const.el --- Global constants and system checks -*- lexical-binding: t; -*-

;;; Commentary:
;; Defines user information and system-specific constants used throughout the configuration.

;;; Code:

;; --- User Information ---
(setq-default user-full-name "0xRoot_7"
              user-mail-address "test@example.com")

;; --- System Predicates ---
(defconst my-sys/windows-p (eq system-type 'windows-nt)
  "Are we running on a Windows NT system?")

(defconst my-sys/linux-p (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst my-sys/mac-p (eq system-type 'darwin)
  "Are we running on a macOS system?")


;; --- Capability Predicates ---
(defconst my-cap/python3-p (executable-find "python3")
  "Is the `python3` executable available in the system PATH?")

(defconst my-cap/pip3-p (executable-find "pip3")
  "Is the `pip3` executable available in the system PATH?")

(defconst my-cap/clangd-p (executable-find "clangd")
  "Is the `clangd` language server available in the system PATH?")

;; Ensure `display-graphic-p` is evaluated correctly; it's a function.
;; `my-cap/eaf-ready-p` now correctly reflects the dependencies.
(defconst my-cap/eaf-ready-p
  (and (display-graphic-p)    ; Check if Emacs is running in a graphical display
       my-cap/python3-p
       my-cap/pip3-p)
  "Is the environment ready for the Emacs Application Framework (EAF)?")

(provide 'init-const)
;;; init-const.el ends here
