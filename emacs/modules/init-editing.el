;;; init-editing.el --- Text Editing Enhancements Configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;; This file configures packages that enhance text editing features like
;; indentation, formatting, highlighting, and common editing actions.
;;
;;; Code:

;; --- 1. Aggressive Indent (Automatic Indentation) ---
(use-package aggressive-indent
  :straight t
  :init
  ;; Add global mode activation to :init for early availability.
  (global-aggressive-indent-mode 1)
  :config
  ;; Exclude modes where aggressive indentation might interfere.
  (add-to-list 'aggressive-indent-excluded-modes 'minibuffer-mode)
  ;; Add specific conditions to prevent indentation, particularly for C-like languages.
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'c++-mode)
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         (thing-at-point 'line)))))
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'csharp-mode)
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         (thing-at-point 'line)))))
  ;; You might want to consider `aggressive-indent-skip-comments t`
  ;; if auto-indenting comments is an issue.
  )

;; --- 2. Color Identifiers Mode (Semantic Highlighting) ---
;; Highlights instances of the same identifier in the buffer.
(use-package color-identifiers-mode
  :straight t
  :hook (after-init . global-color-identifiers-mode) ; Activate globally after Emacs init.
  :config
  (setq color-identifiers:recoloring-delay 1) ; Reduce delay for faster recoloring.
  )

;; --- 3. Dtrt-indent (Detect Real Tab vs. Space Indentation) ---
;; Automatically detects and applies file indentation style (tabs vs. spaces).
;; It's a dependency for some modes, but often used directly.
(use-package dtrt-indent
  :straight t
  :hook (after-change-major-mode . dtrt-indent-mode) ; Enable dtrt-indent-mode on major mode change
  :config
  ;; You might want to set dtrt-indent-global-h-file-modes for header files.
  )

;; --- 4. Fix Word (Case Conversion) ---
;; Quickly change case of words.
(use-package fix-word
  :straight t
  :bind (("M-u" . fix-word-upcase)
         ("M-l" . fix-word-downcase)
         ("M-c" . fix-word-capitalize)))

;; --- 5. Format All (Universal Code Formatter) ---
;; Integrates with external formatters (like `astyle`).
(use-package format-all
  :straight t
  :commands format-all-mode ; Load only when format-all-mode is activated.
  :hook (prog-mode . format-all-mode) ; Enable in programming modes.
  :config
  ;; Define external formatters for different languages.
  ;; Ensure 'astyle' is installed and in your PATH.
  (setq-default format-all-formatters
                '(("C"    (astyle "--mode=allman"))
                  ("C++"  (astyle "--mode=allman"))
                  ("C#"   (astyle "--mode=allman"))
                  ("Java" (astyle "--mode=java"))))
  (global-set-key (kbd "C-c f") 'format-all-buffer)
  )

;; --- 6. Highlight Defined (Highlighting Defined Functions/Variables) ---
;; Highlights defun/defvar forms in Emacs Lisp.
(use-package highlight-defined
  :straight t
  :hook (emacs-lisp-mode . highlight-defined-mode))

;; --- 7. Highlight Indent Guides (Visual Indentation Guides) ---
;; Provides visual cues for indentation levels.
(use-package highlight-indent-guides
  :straight t
  :hook (prog-mode . highlight-indent-guides-mode) ; Enable in programming modes.
  :config
  (setq highlight-indent-guides-method 'bitmap)     ; Use bitmap for better performance/look.
  (setq highlight-indent-guides-auto-enabled nil)   ; Control activation manually.
  ;; Customize face colors for indent guides.
  (set-face-background 'highlight-indent-guides-odd-face "white smoke")
  (set-face-background 'highlight-indent-guides-even-face "peach puff")
  (set-face-foreground 'highlight-indent-guides-character-face "gainsboro"))

;; --- 8. Highlight Thing (Highlighting Symbol at Point) ---
;; Highlights all occurrences of the symbol at point.
(use-package highlight-thing
  :straight t
  :hook (prog-mode . highlight-thing-mode)
  :config
  ;; You might want to configure when it highlights (e.g., only after idle time).
  ;; (setq highlight-thing-delay 0.5)
  )

;; --- 9. Move Dup (Move Lines and Duplicate) ---
;; Commands to move or duplicate lines/regions.
(use-package move-dup
  :straight t
  :init
  (global-move-dup-mode 1) ; Activate globally.
  :bind (("M-p"   . move-dup-move-lines-up)
         ("C-M-p" . move-dup-duplicate-up)
         ("M-n"   . move-dup-move-lines-down)
         ("C-M-n" . move-dup-duplicate-down)))

;; --- 10. Multiple Cursors (Simultaneous Editing) ---
;; Edit multiple locations at once.
(use-package multiple-cursors
  :straight t
  :bind (("C-S-c C-S-c" . mc/edit-lines)        ; Edit multiple lines at once.
         ("C->"         . mc/mark-next-like-this) ; Mark next occurrence.
         ("C-<"         . mc/mark-previous-like-this) ; Mark previous occurrence.
         ("C-c C-<"     . mc/mark-all-like-this)))    ; Mark all occurrences.

;; --- 11. Yasnippet (Code Snippets) ---
;; Dynamic code snippet expansion.
(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1) ; Enable yasnippet globally early on.
  :hook ((prog-mode LaTeX-mode org-mode markdown-mode) . yas-minor-mode)
  :bind
  ;; Bind a key to explicitly expand from trigger key if auto-expansion is not desired.
  (:map yas-minor-mode-map
        ("C-c C-n" . yas-expand-from-trigger-key))
  ;; Bind TAB for snippet expansion, handled by a smarter function.
  (:map yas-keymap
        ("TAB"     . smarter-yas-expand-next-field)
        ([(tab)] . smarter-yas-expand-next-field))
  :config
  ;; Specify snippet directories. Ensure these paths are correct.
  (setq yas-snippet-dirs
        (list
         ;; Add a relative path for personal snippets within your Emacs config:
         (expand-file-name "snippets/" user-emacs-directory)
         ;; Add other snippet collections as needed.
         ;; (file-truename "/path/to/some/collection/")
         ))
  (yas-reload-all) ; Reload all snippets after configuration.

  ;; Custom function to handle TAB intelligently for Yasnippet.
  ;; This tries to expand a snippet; if no snippet, it moves to the next field.
  (defun smarter-yas-expand-next-field ()
    "Try to `yas-expand' then `yas-next-field' at current cursor position."
    (interactive)
    (let ((old-point (point))
          (old-tick (buffer-chars-modified-tick)))
      (yas-expand)
      (when (and (eq old-point (point))
                 (eq old-tick (buffer-chars-modified-tick)))
        (ignore-errors (yas-next-field)))))
  )

;; --- 12. Yasnippet Snippets (Collection of Common Snippets) ---
(use-package yasnippet-snippets
  :straight t
  :after yasnippet) ; Load after yasnippet itself.

;; --- 13. Zoom (Window Zooming) ---
;; Temporarily maximize the current window.
(use-package zoom
  :straight t
  :config
  (zoom-mode t)) ; Enable global keybindings for zooming.

;; --- 14. ConfMode (Configuration File Mode) ---
;; A generic mode for configuration files.
(use-package conf-mode
  :straight nil ; Built-in mode.
  :hook (conf-mode . awesome-pair-mode) ; Enable awesome-pair in conf-mode.
  ;; Bindings that are specific to `conf-mode` and rely on `awesome-pair-mode`
  ;; should be defined within the `conf-mode-map` here.
  :bind (:map conf-mode-map
              ("M-D" . awesome-pair-kill)
              ("SPC" . awesome-pair-space)
              ("="   . awesome-pair-equal)
              ("M-F" . awesome-pair-jump-right)
              ("M-B" . awesome-pair-jump-left)))

(provide 'init-editing)
;;; init-editing.el ends here
