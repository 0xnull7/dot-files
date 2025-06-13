;;; init-indent.el --- Indentation and Visual Guides Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures global indentation settings for Emacs,
;; including tab preferences, `electric-indent-mode` behavior,
;; and integrates `indent-bars` for visual indentation cues.

;;; Code:

;; --- 1. `indent-bars` (Visual Indentation Guides) ---
(use-package indent-bars
  :straight nil
  :load-path (lambda () (expand-file-name "site-elisp/indent-bars" user-emacs-directory))
  :hook ((prog-mode yaml-mode) . indent-bars-mode) ; Enable in programming and YAML modes.
  :custom
  (indent-bars-treesit-support t)           ; Enable Tree-sitter support if available.
  (indent-bars-no-descend-string t)         ; Don't descend into strings when drawing bars.
  (indent-bars-treesit-ignore-blank-lines-types '("module")) ; Ignore specific Tree-sitter node types.
  (indent-bars-treesit-wrap                ; Define node types where bars should wrap.
   '((python argument_list parameters
             list list_comprehension
             dictionary dictionary_comprehension
             parenthesized_expression subscript)))
  (indent-bars-pattern ". . . . ")          ; Pattern for the indentation bars.
  (indent-bars-width-frac 0.25)             ; Width of the bar relative to column width.
  (indent-bars-pad-frac 0.2)                ; Padding of the bar.
  (indent-bars-zigzag 0.1)                  ; Add a subtle zigzag effect for visual separation.
  (indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)) ; Color bars by depth.
  (indent-bars-highlight-current-depth '(:pattern "." :pad 0.1 :width 0.45)) ; Highlight current depth.
  )

;; --- 2. Global Indentation Configuration (`IndentConfig`) ---
;; Set universal indentation preferences.
(setq-default indent-tabs-mode nil) ; Prefer spaces over tabs for indentation by default.
(setq-default indent-line-function 'insert-tab) ; Still use `insert-tab` if `indent-tabs-mode` is nil.
                                                ; This effectively means `TAB` inserts spaces.
(setq-default tab-width 4)         ; Set the visual width of a tab and indentation step to 4 spaces.
(setq-default c-basic-offset 4)    ; Basic offset for C-like modes.
(setq-default js-switch-indent-offset 4) ; Indentation for JavaScript `switch` statements.

;; C-like mode specific indentation adjustments.
(c-set-offset 'comment-intro 0)    ; No extra indent for comments.
(c-set-offset 'innamespace 0)      ; No extra indent for content within namespaces.
(c-set-offset 'case-label '+)     ; Indent `case` labels relative to `switch`.
(c-set-offset 'access-label 0)     ; Indent access specifiers (public:, private:) at column 0.
(c-set-offset (quote cpp-macro) 0 nil) ; No indent for C++ preprocessor macros.

;; Smart `electric-indent-mode` management.
(defun smart-electric-indent-mode-setup ()
  "Disable `electric-indent-mode` in specific buffers and ensure it's
enabled otherwise. Hook this to `prog-mode-hook` or `major-mode-hook`."
  (cond ((and (eq electric-indent-mode t)
              (member major-mode '(erc-mode text-mode))) ; Add other modes as needed
         (electric-indent-mode -1)) ; Use -1 to disable without killing mode
        ((eq electric-indent-mode nil)
         (electric-indent-mode 1)))) ; Enable if it's currently nil

;; It's generally better to hook `smart-electric-indent-mode-setup` to
;; `major-mode-hook` or `prog-mode-hook` rather than `post-command-hook`
;; to avoid running it after every command.
(add-hook 'major-mode-hook #'smart-electric-indent-mode-setup)
;; Or, more specifically for programming modes:
;; (add-hook 'prog-mode-hook #'smart-electric-indent-mode-setup)


(provide 'init-indent)
;;; init-indent.el ends here
