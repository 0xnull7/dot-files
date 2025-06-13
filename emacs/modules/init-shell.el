;;; init-shell.el --- Integrated Shell and Terminal Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures `shell-here`, `multi-term`, `term-keys`, and
;; `exec-path-from-shell` for a comprehensive shell and terminal
;; experience directly within Emacs.

;;; Code:

;; --- 1. `shell-here` (Open Shell in Current Directory) ---
(use-package shell-here
  :straight t
  :bind ("M-~" . shell-here)
  :config
  ;; If on a Linux system, ensure the default shell for `shell-here` is Zsh.
  (when (bound-and-true-p *sys/linux*)
    (setq explicit-shell-file-name "/bin/zsh"))
  ;; Set the default shell for `shell-mode` (M-x shell).
  ;; This is separate from `explicit-shell-file-name` used by `shell-here`.
  ;; (setq shell-file-name "/bin/zsh")
  )

;; --- 2. `multi-term` (Multiple Terminals) ---
(use-package multi-term
  :straight nil
  :load-path (lambda () (expand-file-name "site-elisp/multi-term" user-emacs-directory))
  :commands (multi-term)
  :bind
  ;; Global binding to create/switch to a terminal.
  ("M-$" . multi-term)
  ;; Add the same binding within `dired-mode` for convenience.
  (:map dired-mode-map ("M-$" . multi-term))

  :custom
  (multi-term-program (executable-find "zsh"))
  ;; Rebind keys within `term-mode` to behave more like a standard shell.
  ;; `term-bind-key-alist` is applied globally to `term-mode` buffers.
  (term-bind-key-alist
   '(("C-c C-c" . term-interrupt-subjob) ; Interrupt current job (e.g., Ctrl-C).
     ("C-c C-e" . term-send-esc)        ; Send Esc to the terminal.
     ("C-p" . previous-line)            ; Move up a line.
     ("C-n" . next-line)                ; Move down a line.
     ("C-m" . term-send-return)         ; Send Enter (Return).
     ("C-y" . term-paste)               ; Paste from kill ring.
     ("C-v" . scroll-up-command)        ; Scroll page up.
     ("M-v" . scroll-down-command)      ; Scroll page down.
     ("M-f" . term-send-forward-word)   ; Move forward by word.
     ("M-b" . term-send-backward-word)  ; Move backward by word.
     ("M-o" . term-send-backspace)      ; Send Backspace.
     ("M-p" . term-send-up)             ; Send Up arrow.
     ("M-n" . term-send-down)           ; Send Down arrow.
     ("M-M" . term-send-forward-kill-word) ; Kill word forward.
     ("M-N" . term-send-backward-kill-word) ; Kill word backward.
     ("<C-backspace>" . term-send-backward-kill-word) ; Kill word backward with Ctrl-Backspace.
     ("<M-backspace>" . term-send-backward-kill-word) ; Kill word backward with Alt-Backspace.
     ("M-r" . term-send-reverse-search-history) ; Reverse search history.
     ("M-d" . term-send-delete-word)    ; Delete word forward.
     ("M-," . term-send-raw)            ; Send raw escape sequence.
     ("M-." . comint-dynamic-complete))) ; Dynamic completion (e.g., for file names).
  )

;; --- 3. `term-keys` (Enhanced Keybindings for Terminal Mode) ---
; (use-package term-keys
;   :straight t
;   :if (not (display-graphic-p))
;   :config
;   (term-keys-mode t)
;   )

;; --- 4. `exec-path-from-shell` (Inherit Shell Environment) ---
;; Ensures Emacs inherits your shell's PATH and other environment variables.
;; This is crucial for Emacs to find executables like `rg`, `go`, `node`, etc.
(use-package exec-path-from-shell
  :straight t ; Ensure `exec-path-from-shell` is managed and installed by straight.el.
  ;; Only enable on macOS and similar systems where Emacs GUI might not inherit shell path.
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize) ; Initialize the package to pull environment variables.
  ;; If you want to pull ALL environment variables, not just PATH:
  ;; (exec-path-from-shell-initialize '("PATH" "LANG" "LC_ALL" "GOPATH" "NVM_DIR" ...))
  )

(provide 'init-shell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
