;;; init-global-config.el --- Global Emacs Configuration Settings -*- lexical-binding: t; -*-

;;; Commentary:
;; This file defines fundamental, global configurations for Emacs,
;; including utility packages, keybindings, coding systems, whitespace handling,
;; history, backup settings, and other general behaviors that apply throughout
;; the Emacs environment.

;;; Code:

;; --- 1. Core Utilities ---

;; `sudo-edit`: Edit files with sudo privileges directly from Emacs.
(use-package sudo-edit :straight t)

;; `recentf`: Keeps a list of your recently visited files for quick access.
(use-package recentf
  :straight nil ; `recentf` is a built-in Emacs feature, no need for straight.el
  :hook (after-init . recentf-mode) ; Enable recentf-mode globally after Emacs initializes
  :custom
  (recentf-max-menu-items 25) ; Number of items to show in the Recentf menu
  (recentf-max-saved-items 200) ; Max number of items to keep in the history file
  (recentf-auto-cleanup t) ; Automatically remove non-existent files from list

  ;; Exclude common temporary, history, or configuration files from recentf list
  (recentf-exclude '(
                      "COMMIT_EDITMSG"           ; Git commit message file
                      "\\(/\\|^\\)ido\\..*\\'"  ; Ido history files
                      "\\(/\\|^\\)tramp#.*"     ; TRAMP temporary files
                      "\\.~[0-9]+$"             ; Backup files (e.g., file.~1~)
                      "\\(#.*#\\)$"             ; Auto-save files (e.g., #file.txt#)
                      "\\(/\\|^\\)\\.cache\\(/\\|\\'\\)" ; .cache directory
                      "\\(/\\|^\\)\\.cask\\(/\\|\\'\\)"  ; .cask directory
                      "\\(/\\|^\\)\\.elfeed\\(/\\|\\'\\)" ; .elfeed directory
                      "\\(/\\|^\\)bookmarks\\'"  ; Emacs bookmark file
                      "\\(/\\|^\\)recentf\\'"    ; The recentf file itself
                      "\\(/\\|^\\)undo-tree-hist\\'" ; Undo-tree history file
                      (lambda (file) (string-prefix-p (expand-file-name package-user-dir) file)) ; Exclude package directory
                      ))
  :config
  ;; Save the recentf list periodically (e.g., every 5 minutes)
  (run-at-time nil (* 5 60) 'recentf-save-list)
  )

;; `save-place-mode`: Remembers the cursor position and window configuration
;; for each file, restoring them the next time you open the file.
(use-package saveplace
  :straight nil ; saveplace is a built-in Emacs feature
  :hook (after-init . save-place-mode) ; Enable save-place-mode globally after init
  :custom
  ;; Store the saveplace data file in your Emacs configuration directory
  (save-place-file (expand-file-name "saveplace" user-emacs-directory)))

;; `auto-revert`: Automatically reloads buffers when their corresponding files
;; on disk have changed (e.g., modified by an external tool or Git pull).
(setq global-auto-revert-file-modes t) ; Revert all file modes (not just text files)
(setq auto-revert-interval 2) ; Check for changes every 2 seconds
(setq auto-revert-verbose nil) ; Suppress messages when auto-reverting

;; --- 2. Keybinding Definitions ---

;; Unbind potentially conflicting or unwanted global keybindings.
(global-unset-key (kbd "C-z"))      ; Often used for suspend, or can be repurposed
(global-unset-key (kbd "M-z"))      ; Unbound by default, but good to be explicit
(global-unset-key (kbd "M-m"))      ; Unbound by default, or for context-menu
(global-unset-key (kbd "C-x C-z"))  ; Suspend Emacs, unbinding frees it
(global-unset-key (kbd "M-/"))      ; `dabbrev-expand`, often rebound or superseded by company-mode

;; Custom function to save all modified file-visiting buffers.
(defun my/save-all-buffers ()
  "Save all modified file-visiting buffers without prompting."
  (interactive)
  (save-some-buffers t))

;; Bind a custom function to save all buffers.
(global-set-key (kbd "C-x C-S-s") #'my/save-all-buffers)

;; --- 3. UTF-8 Coding System Configuration ---
;; Set Emacs to use UTF-8 as the primary coding system for most interactions.
;; This block is crucial for proper display and handling of international characters.
;; The `my-sys/win32-p` variable is assumed to be defined elsewhere (e.g., init-const.el).
(unless (bound-and-true-p my-sys/win32-p)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first on graphic displays.
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; --- 4. Whitespace Handling ---
;; Provides a custom function to remove trailing whitespace from all lines
;; except the current one, and manages its automatic application.

(defun delete-trailing-whitespace-except-current-line ()
  "An alternative to `delete-trailing-whitespace'.
  The original function deletes trailing whitespace of the current line.
  This function deletes trailing whitespace from all lines except the current one."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      ;; Narrow to region before current line and delete whitespace
      (when (< (point-min) begin)
        (save-restriction
          (narrow-to-region (point-min) begin)
          (delete-trailing-whitespace)
          (widen)))
      ;; Narrow to region after current line and delete whitespace
      (when (> (point-max) end)
        (save-restriction
          (narrow-to-region end (point-max))
          (delete-trailing-whitespace)
          (widen))))))

;; Conditionally remove trailing whitespace for specific major modes.
;; Exclude modes like `diff-mode` where trailing whitespace might be significant.
(defun smart-delete-trailing-whitespace ()
  "Invoke `delete-trailing-whitespace-except-current-line' on selected major modes only."
  (unless (member major-mode '(diff-mode))
    (delete-trailing-whitespace-except-current-line)))

;; Add `smart-delete-trailing-whitespace` to `before-save-hook` during startup.
;; This ensures trailing whitespace is cleaned before saving, unless toggled off.
(add-hook 'before-save-hook #'smart-delete-trailing-whitespace)

;; Function to toggle automatic trailing whitespace removal from `before-save-hook`.
(defun toggle-auto-trailing-ws-removal ()
  "Toggle automatic trailing whitespace removal before saving."
  (interactive)
  (if (member #'smart-delete-trailing-whitespace before-save-hook)
      (progn
        (remove-hook 'before-save-hook #'smart-delete-trailing-whitespace)
        (message "Disabled auto remove trailing whitespace."))
    (add-hook 'before-save-hook #'smart-delete-trailing-whitespace)
    (message "Enabled auto remove trailing whitespace.")))

;; Replace selected region on insert (typing overwrites selection).
(setq delete-selection-mode t)

;; --- 5. History Settings ---
;; Set history length for minibuffer and command history.
(setq history-length 500)

;; --- 6. Backup and Auto-Save Files Configuration ---
;; This section centralizes settings to prevent Emacs from cluttering your
;; project directories with backup (~) and auto-save (#) files.

;; Store all backup files in a dedicated directory within your Emacs config.
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

;; Create the backup directory if it doesn't exist.
(unless (file-directory-p (cdr (car backup-directory-alist)))
  (make-directory (cdr (car backup-directory-alist)) t))

;; Store auto-save files in a dedicated directory as well.
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))

;; Configure backup behavior:
(setq backup-by-copying t)       ; Copy the file before writing changes
(setq delete-old-versions t)     ; Delete older backup versions
(setq kept-new-versions 6)       ; Keep 6 newest versions of backups
(setq kept-old-versions 2)       ; Keep 2 oldest versions (if `version-control` is active)
(setq version-control t)         ; Use numeric suffixes for backup versions (e.g., file.~1~, file.~2~)

;; Prevent Emacs from creating lock files (e.g., #foo.el#) in project directories.
(setq create-lockfiles nil)

;; Disable desktop save mode, which saves buffer list/window config on exit/startup.
;; Uncomment if you prefer to *not* restore your previous Emacs session.
(setq desktop-save-mode nil)


;; --- 7. Miscellaneous Global Settings ---

;; Confirm before killing Emacs (uncomment if you want this prompt).
;; (setq confirm-kill-emacs 'y-or-n-p)

;; Automatically kill all active processes when closing Emacs without asking.
(setq confirm-kill-processes nil)

;; Turn off cursor alarms (visual bells) - prevents screen flashing on errors.
(setq ring-bell-function 'ignore)

;; Show keystrokes in progress instantly in the echo area (0.1 seconds delay).
(setq echo-keystrokes 0.1)

;; Better compilation behavior.
(setq compilation-always-kill t)      ; Kill previous process before starting new one.
(setq compilation-ask-about-save nil) ; Save all buffers on `compile` without asking.
(setq compilation-scroll-output t)    ; Auto-scroll compilation output.

;; Suppress `defadvice` redefinition warnings (useful for some packages).
(setq ad-redefinition-action 'accept)

;; Enable `so-long` mode for mitigating slowness on very long lines (Emacs 28+).
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;; Enable `erase-buffer` function (it's often disabled by default for safety).
(put 'erase-buffer 'disabled nil)

;; Automatically assign text modes to specific file extensions.
;; Add common extensions that Emacs might not auto-detect correctly.
(add-to-list 'auto-mode-alist '("\\.in\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.args\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.bb\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.bbclass\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))

;; Always show the minibuffer (e.g., for completion candidates)
(setq enable-recursive-minibuffers t)

;; Set the default directory when Emacs starts (uncomment and customize if needed)
;; (setq default-directory "~/")

(provide 'init-global-config)
;;; init-global-config.el ends here
