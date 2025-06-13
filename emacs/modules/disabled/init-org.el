;;; init-org.el --- Org Mode and Org-roam Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures Org mode, Org-roam, and associated packages like
;; toc-org, htmlize, ox-gfm, and plantuml-mode, for enhanced note-taking,
;; task management, and knowledge organization.

;;; Code:

;; --- 1. `org` (Core Org Mode) ---
;; `org` is built-in and not installed via `straight.el`.
(use-package org
  :straight nil
  :defer t
  :bind
  ;; Global keybindings for common Org actions.
  ("C-c l" . org-store-link) ; Store a link to the current location.
  ("C-c a" . org-agenda)     ; Open the Org Agenda.
  ("C-c c" . org-capture)    ; Capture new notes/tasks.
  ;; Keybindings specific to `org-mode`.
  (:map org-mode-map
        ("C-c C-p" . eaf-org-export-to-pdf-and-open) ; Assuming `eaf` is configured elsewhere.
        ("C-c ;" . nil) ; Unbinds C-c ; in Org mode if it conflicts with something.
        )

  :custom
  ;; Log the time when a TODO item is marked DONE.
  (org-log-done 'time)
  ;; Your geographic coordinates for sun-related functions (e.g., `sunrise-sunset`).
  (calendar-latitude 43.65107)    ; Toronto, Canada (your default)
  (calendar-longitude -79.347015) ; Toronto, Canada (your default)
  ;; Export backends that Org mode can use.
  (org-export-backends '(ascii html icalendar latex md odt))
  ;; Enable Org speed commands (single-key commands for headlines).
  (org-use-speed-commands t)
  ;; Don't ask for confirmation when evaluating Babel source blocks.
  (org-confirm-babel-evaluate nil)
  ;; LaTeX listing options for code blocks (using `listings` package by default).
  (org-latex-listings-options '(("breaklines" "true")))
  (org-latex-listings t) ; Enable LaTeX listings for code blocks.
  ;; Number of days in advance to warn for deadlines.
  (org-deadline-warning-days 7)
  ;; Custom TODO keyword sequence.
  (org-todo-keywords
   '((sequence "TODO" "IN-PROGRESS" "REVIEW" "|" "DONE" "CANCELED")))
  ;; Where to display the Org agenda buffer.
  (org-agenda-window-setup 'other-window)
  ;; LaTeX PDF compilation process, useful for complex documents with listings.
  (org-latex-pdf-process
   '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; Set the default directory for Org agenda files.
  ;; Note: This should ideally be outside the `when` block to set a default,
  ;; and the `when` block for `org-agenda-files` is fine for ensuring it exists.
  (org-directory (expand-file-name "~/org/")) ; Standard for Org files.

  :custom-face
  ;; Customize the color of the current time line in the Org agenda.
  (org-agenda-current-time ((t (:foreground "spring green"))))

  :config
  ;; Add `listings` package to LaTeX export for syntax highlighting.
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  ;; Load `org-tempo` for Org mode's quick insert templates (e.g., `<s`).
  ;; Check Org version to ensure compatibility.
  (unless (version< org-version "9.2")
    (require 'org-tempo))

  ;; Set Org agenda files directory if it exists.
  (when (file-directory-p (expand-file-name "~/org/agenda/"))
    (setq org-agenda-files (list (expand-file-name "~/org/agenda/"))))
  ;; Alternatively, consider `org-agenda-files` as a list of actual files:
  ;; (setq org-agenda-files (directory-files-recursively "~/org/agenda/" "\\.org$"))

  ;; Configure Org Babel to enable specific languages for code execution.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (C . t)
     (python . t)
     (plantuml . t)
     ;; Add other languages you use, e.g.:
     ;; (emacs-lisp . t)
     ;; (shell . t)
     ;; (sqlite . t)
     ))

  ;; Custom function to enable Minted for LaTeX export syntax highlighting.
  ;; Call this function interactively before exporting if you want Minted.
  (defun org-export-toggle-syntax-highlight ()
    "Setup variables to turn on syntax highlighting using `minted` when calling `org-latex-export-to-pdf'.
This function needs to be called interactively before export."
    (interactive)
    (setq-local org-latex-listings 'minted)
    (add-to-list 'org-latex-packages-alist '("newfloat" "minted")))

  ;; Custom function to insert a vertical hline for Org tables (LaTeX export).
  (defun org-table-insert-vertical-hline ()
    "Insert a #+attr_latex: :align |c|c|c| to the current buffer, default align to |c|c|c|.
Adjust the alignment string if necessary."
    (interactive)
    (insert "#+attr_latex: :align |c|c|c|")))

;; --- 2. `org-roam` (Zettelkasten-style Note Management) ---
(use-package org-roam
  :straight t ; Ensure `org-roam` is managed and installed by straight.el.
  :after org ; Load `org-roam` after `org` mode is available.
  :init
  ;; Set Org-roam directory early if it exists.
  (when (file-directory-p (expand-file-name "~/org/roam/"))
    (setq org-roam-directory (file-truename (expand-file-name "~/org/roam"))))
  :custom
  ;; Template for displaying Org-roam nodes (e.g., in `org-roam-node-find`).
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  ;; Enable Org-roam completion everywhere (e.g., in `org-mode` buffers).
  (org-roam-completion-everywhere t)

  :bind
  ;; Keybindings for Org-roam commands.
  ("C-c n l" . org-roam-buffer-toggle) ; Toggle the Org-roam buffer.
  ("C-c n f" . org-roam-node-find)     ; Find an Org-roam node.
  ("C-c n i" . org-roam-node-insert)   ; Insert a new Org-roam node.
  ("C-c n h" . org-id-get-create)      ; Get or create an Org-ID for the current headline.
  ;; You might also want to bind `org-roam-dailies-capture-today` for daily notes.
  ;; ("C-c n d" . org-roam-dailies-capture-today)

  :config
  ;; Enable automatic synchronization of the Org-roam database.
  (org-roam-db-autosync-mode))

;; --- 3. `toc-org` (Table of Contents Generation) ---
(use-package toc-org
  :straight t ; Ensure `toc-org` is managed and installed by straight.el.
  :hook (org-mode . toc-org-mode) ; Enable `toc-org-mode` automatically in Org buffers.
  )

;; --- 4. `htmlize` (Syntax Highlighting for HTML Export) ---
;; Used by Org mode for exporting code blocks with syntax highlighting to HTML.
(use-package htmlize
  :straight t ; Ensure `htmlize` is managed and installed by straight.el.
  :defer t ; Defer loading until needed by export functions.
  )

;; --- 5. `ox-gfm` (GitHub Flavored Markdown Export) ---
;; Enables exporting Org files to GitHub Flavored Markdown.
(use-package ox-gfm
  :straight t ; Ensure `ox-gfm` is managed and installed by straight.el.
  :defer t ; Defer loading until needed for GFM export.
  )

;; --- 6. `plantuml-mode` (PlantUML Diagram Support) ---
;; Integrates PlantUML for creating diagrams within Org files.
(use-package plantuml-mode
  :straight t ; Ensure `plantuml-mode` is managed and installed by straight.el.
  :mode "\\.puml\\'" ; Auto-activate for .puml files if you edit them directly.
  :hook (org-mode . plantuml-mode) ; Enable plantuml-mode in Org buffers for source blocks.
  :custom
  ;; Path to your PlantUML JAR file. IMPORTANT: Update this path!
  (org-plantuml-jar-path (expand-file-name "~/tools/plantuml/plantuml.jar")) ; <<-- CHANGE THIS
  )

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
