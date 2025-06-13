;;; init-search.el --- Enhanced Search and Navigation Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures a powerful interactive completion and search environment
;; primarily using the `consult` framework, complemented by `vertico` for the UI,
;; `orderless` for flexible matching, `color-rg` for interactive ripgrep,
;; and `find-file-in-project` for project-wide file finding.

;;; Code:

;; --- 1. `vertico` (A Minimal and Performant Completion UI) ---
;; Vertico is the recommended completion UI for Consult. It replaces the default
;; Emacs completion UI and provides a simple, fast, and highly configurable interface.
(use-package vertico
  :straight t
  :init
  (vertico-mode 1) ; Enable Vertico globally

  :config
  (setq vertico-count 10) ; Number of candidates to display
  (setq vertico-cycle t) ; Cycle through candidates when at ends
  (setq vertico-resize t) ; Automatically resize completion window
  )

;; --- 2. `orderless` (Flexible Matching Style for Completions) ---
;; Orderless provides powerful, space-separated pattern matching for completion.
;; For example, "foo bar" will match "foobar", "foo-baz-bar", "bar baz foo", etc.
;; It works seamlessly with Vertico and Consult.
(use-package orderless
  :straight t
  :init
  ;; Set orderless as the default completion style.
  (setq completion-styles '(orderless basic))
  ;; Configure orderless dispatchers for different matching behaviors.
  (setq orderless-skip-highlight-regexp "^(?:.*\\s-+)?\\(?:[[:punct:]]\\|\\s-\\)+\\s-*")
  (setq
   completion-category-defaults nil
   completion-category-overrides '((file (styles partial-completion)))) ; For files, partial is often better
  ;; Optional: Configure orderless-style-dispatchers for advanced matching
  ;; (setq orderless-style-dispatchers
  ;;       '(orderless-literal-dispatcher
  ;;         orderless-regexp-dispatcher
  ;;         orderless-initialism-dispatcher
  ;;         orderless-flex-dispatcher))
  )

;; --- 3. `consult` (Extensible Search and Select Commands) ---
;; Consult provides powerful interactive search and selection commands that
(use-package consult
  :straight t
  :bind (
         ;; C-c bindings (typically mode-specific or custom)
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)

         ;; C-x bindings (often replace default Emacs commands)
         ("C-x M-:" . consult-complex-command)   ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)          ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)    ;; orig. project-switch-to-buffer

         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)        ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)

         ;; Other custom bindings
         ("M-y" . consult-yank-pop)              ;; orig. yank-pop (replaces counsel-yank-pop)

         ;; M-g bindings in `goto-map' (for navigation within buffers)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)             ;; Alternative: consult-flycheck (if using flycheck)
         ("M-g g" . consult-goto-line)           ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)         ;; orig. goto-line
         ("M-g o" . consult-outline)             ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)

         ;; M-s bindings in `search-map' (for text search)
         ("M-s d" . consult-find)                ;; Alternative: consult-fd (requires 'fd')
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)             ;; Use ripgrep with Consult
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)

         ;; Isearch integration (when in isearch mode)
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)        ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)      ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                 ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)           ;; needed by consult-line to detect isearch

         ;; Minibuffer history (when in minibuffer)
         :map minibuffer-local-map
         ("M-s" . consult-history)               ;; orig. next-matching-history-element
         ("M-r" . consult-history))              ;; orig. previous-matching-history-element

  :hook (completion-list-mode . consult-preview-at-point-mode) ; Enable preview in completion list
  :init
  ;; Advice to use consult-register-window for register preview
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any) ; Debounce preview for themes
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any)) ; Debounce preview for other commands

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ; Set the key for narrowing search results
  )

;; --- 4. `color-rg` (Interface to Ripgrep with Colors) ---
;; This package provides an interactive interface to ripgrep with colorized output.
;; While 'consult-ripgrep' is part of consult, 'color-rg' offers a different
;; UI/workflow that some users might prefer or use in specific contexts.
(use-package color-rg
  :straight nil ; This is likely a local installation or not managed by straight.el
  :load-path (lambda () (expand-file-name "site-elisp/color-rg" user-emacs-directory))
  :if (executable-find "rg") ; Load only if ripgrep is found
  :bind
  ("C-M-s" . color-rg-search-input) ; Bind C-M-s to start an interactive color-rg search.
  :config
  ;; You can add custom 'color-rg' settings here, e.g.:
  ;; (setq color-rg-global-arguments '("--max-columns=200"))
  )

;; --- 5. `find-file-in-project` (Project-Wide File Finding) ---
;; This package is a standalone utility for finding files by name within a project.
;; It complements consult's file finding capabilities (e.g., consult-find/fd)
;; by offering a different interface or specific project heuristics.
(use-package find-file-in-project
  :straight t
  :if (executable-find "find") ; Load only if 'find' is available
  :init
  (when (executable-find "fd") ; Prefer 'fd' (rust-based find) if available
    (setq ffip-use-rust-fd t))
  :bind
  ("C-z o" . ffap) ; Find file at point
  ("C-z p" . ffip) ; Find file in project (interactive)
  :config
  ;; Example: Configure which files/extensions to ignore.
  ;; (add-to-list 'ffip-common-ignored-extensions '("~" "#" ".bak" ".orig"))
  )

(provide 'init-search)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-search.el ends here
