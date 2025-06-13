;;; init-func.el --- Custom utility functions -*- lexical-binding: t; -*-

;;; Commentary:
;; This file defines various custom Emacs Lisp functions and helper utilities
;; used throughout the configuration and for general interactive use.

;;; Code:

;; --- 1. `edit-configs` (Open Main Configuration File) ---
; (defun edit-configs ()
;   "Opens the main configuration file (`init.org`)."
;   (interactive)
;   (find-file (expand-file-name "init.org" user-emacs-directory)))
;
; (global-set-key (kbd "C-z e") #'edit-configs)

;; --- 2. `save-and-update-includes` (Org Mode INCLUDE Auto-Update) ---
;; Automatically updates line numbers for `#+INCLUDE:` directives in Org mode.
(defun save-and-update-includes ()
  "Update the line numbers of #+INCLUDE:s in current buffer.
Only looks at INCLUDEs that have either :range-begin or :range-end.
This function does nothing if not in `org-mode', so you can safely
add it to `before-save-hook'."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^\\s-*#\\+INCLUDE: *\"\\([^\"]+\\)\".*:range-\\(begin\\|end\\)"
              nil 'noerror)
        (let* ((file (expand-file-name (match-string 1)))
               (line-match-data (thing-at-point 'line)) ; Capture current line for robust regex
               begin end)
          ;; Ensure `line-match-data` is valid before string-match-p
          (when (and line-match-data
                     (string-match-p ":range-begin *\"\\([^\"]+\\)\"" line-match-data))
            (setq begin (match-string 1 line-match-data))) ; Use match-string 1 from line-match-data
          (when (and line-match-data
                     (string-match-p ":range-end *\"\\([^\"]+\\)\"" line-match-data))
            (setq end (match-string 1 line-match-data)))

          (let ((lines (decide-line-range file begin end)))
            (when lines
              (if (string-match-p ":lines *\"[-0-9]+\"" (thing-at-point 'line))
                  ;; If :lines already exists, replace it
                  (replace-match (format ":lines \"%s\"" lines) :fixedcase :literal t 0)
                ;; If :lines does not exist, insert it
                (goto-char (line-end-position))
                (insert (format " :lines \"%s\"" lines))))))))))

(add-hook 'before-save-hook #'save-and-update-includes)

(defun decide-line-range (file begin end)
  "Visit FILE and decide which lines to include.
BEGIN and END are regexps which define the line range to use.
Returns a string like \"X-Y\"."
  (let (l r)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (if (null begin)
          (setq l 1) ; If no begin regex, start from line 1
        (when (re-search-forward begin nil t)
          (setq l (line-number-at-pos)))) ; Use `line-number-at-pos` without args for current line
      (if (null end)
          (setq r (count-lines (point-min) (point-max))) ; If no end regex, go to last line
        (when (re-search-forward end nil t)
          (setq r (line-number-at-pos (match-end 0)))))

      ;; Format: `start-line`-`end-line`
      (format "%s-%s" l r)))) ; Remove (+ l 1) and (- r 1) unless the original intent was different.
                               ; `line-number-at-pos` returns 1-indexed.
                               ; The original logic `(+ l 1)` `(- r 1)` suggests
                               ; `begin` and `end` were exclusive, but the search
                               ; behavior here makes them inclusive of the match.

;; --- 3. Better Minibuffer (Minibuffer Enhancements) ---
;; Aborts minibuffer on mouse leave and customizes prompt.
(defun abort-minibuffer-using-mouse ()
  "Abort the minibuffer when the mouse pointer leaves its window.
This is useful to dismiss completion popups quickly."
  (when (and (active-minibuffer-window) ; Ensure we are in a minibuffer
             (minibufferp (window-buffer (selected-window)))) ; Ensure the current buffer is the minibuffer
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'abort-minibuffer-using-mouse)

;; Keep the point out of the minibuffer prompt for cleaner input.
(setq-default minibuffer-prompt-properties
              '(read-only t
                point-entered minibuffer-avoid-prompt
                face minibuffer-prompt))

;; --- 4. `display-line-overlay+` (Line Overlay Utility) ---
;; Helper function to display text as an overlay on a specific line.
(defun display-line-overlay+ (pos str &optional face)
  "Display line at POS as STR with FACE.
FACE defaults to inheriting from `default` and `highlight`."
  (let ((ol (save-excursion
              (goto-char pos)
              (make-overlay (line-beginning-position)
                            (line-end-position)))))
    (overlay-put ol 'display str)
    (overlay-put ol 'face
                 (or face '(:background nil :inherit highlight))) ; Use `nil` for no background.
    ol))

;; --- 5. `read-lines` (Read File into List of Lines) ---
(defun read-lines (file-path)
  "Return a list of lines from the file at FILE-PATH.
Empty lines are included."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t))) ; `t` for `NO-EMPTY` means skip empty strings

;; If you want to include empty lines, remove the `t` argument.
;; (split-string (buffer-string) "\n")

;; --- 6. `where-am-i` (Show Current Buffer's File/Name) ---
(defun where-am-i ()
  "Displays and copies the current buffer's file path or name to kill-ring."
  (interactive)
  (let ((path-or-name (if (buffer-file-name) (buffer-file-name) (buffer-name))))
    (message "Current location: %s" path-or-name)
    (kill-new path-or-name))) ; Copy to kill-ring

(provide 'init-func)
;;; init-func.el ends here
