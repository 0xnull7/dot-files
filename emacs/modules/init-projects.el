;;; init-projects.el --- Project and File Tree Management Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures packages for improved project management and file tree navigation,
;; primarily setting up `projectile` and `treemacs` along with their integrations.
;; Focus: Robust loading, best practices for `use-package`, and sensible settings.

;;; Code:

;; --- 1. `projectile` (Project Interaction Library) ---
(use-package projectile
  :straight t
  :init
  ;; Enable projectile globally. Set to `nil` if you want to enable it per-project.
  (projectile-mode +1)
  :bind
  ;; Global keybinding to access Projectile commands.
  ("C-c p" . projectile-command-map)
  :config
  ;; Define paths where Projectile should look for projects.
  ;; Adjust these to your actual development directories.
  (setq projectile-project-search-path '("~/_Dev/" "~/_Dev/_Projects/"))
  ;; Enable caching for faster project switching, especially with many projects.
  (setq projectile-enable-caching t)
  ;; Ignore certain directories when searching for projects.
  (setq projectile-ignored-project-cum-dirs '("node_modules" ".venv" ".cask" ".git"))
  )

;; --- 2. `treemacs` (File Tree Explorer) ---
(use-package treemacs
  :straight t
    :init
  ;; This block ensures all Treemacs-related initializations
  ;; and minor modes are enabled ONLY after `treemacs` is fully loaded.
  (with-eval-after-load 'treemacs
    ;; Enable core Treemacs minor modes
    (treemacs-follow-mode t)         ; Follow the current file in the tree.
    (treemacs-filewatch-mode t)      ; Watch file system for changes.
    (treemacs-fringe-indicator-mode 'always) ; Always show indicators (e.g., git status).

    ;; Configure Git integration based on executable availability.
    (when (executable-find "git")
      (if treemacs-python-executable ; `treemacs-python-executable` is set by Treemacs if Python is found
          (treemacs-git-mode 'deferred) ; Use deferred git mode if git and python are available.
        (treemacs-git-mode 'simple)))   ; Use simple git mode if only git is available.

    ;; Enable git commit diff mode if Python executable is found.
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    ;; Hide gitignored files.
    (treemacs-hide-gitignored-files-mode t)

    ;; Define global keybindings directly using `define-key`.
    ;; This is a robust way to set keybindings that avoids macro issues
    ;; and ensures the commands exist when the binding is made.
    (define-key global-map (kbd "M-0") #'treemacs-select-window) ; Select the Treemacs window.
    (define-key global-map (kbd "C-x t 1") #'treemacs-delete-other-windows) ; Close other windows, keep Treemacs.
    (define-key global-map (kbd "C-x t t") #'treemacs) ; Open Treemacs (if closed) or raise (if hidden).
    (define-key global-map (kbd "C-x t d") #'treemacs-select-directory) ; Select a directory to open in Treemacs.
    (define-key global-map (kbd "C-x t B") #'treemacs-bookmark) ; Manage Treemacs bookmarks.
    (define-key global-map (kbd "C-x t C-t") #'treemacs-find-file) ; Find file in Treemacs.
    (define-key global-map (kbd "C-x t M-t") #'treemacs-find-tag) ; Find tag in Treemacs.

    ;; If you want Treemacs to start automatically on Emacs launch, uncomment the following:
    ;; (treemacs-start-on-boot)
    ;; Or to display in side window when project root changes:
    ;; (treemacs-add-hook 'treemacs-project-root-changed-hook #'treemacs-display-in-side-window)
    )

  ;; Define keybinding for `winum` integration, if `winum` is used.
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))

  :custom
  ;; Using `:custom` is preferred for user-facing options as it integrates with `M-x customize`.
  (treemacs-collapse-dirs (if treemacs-python-executable 3 0))
  (treemacs-icons 'all-the-icons) ; Requires `all-the-icons` package.
  (treemacs-deferred-git-apply-delay 0.5)
  (treemacs-directory-name-transformer #'identity)
  (treemacs-display-in-side-window t)
  (treemacs-eldoc-display 'simple)
  (treemacs-file-event-delay 2000)
  (treemacs-file-extension-regex treemacs-last-period-regex-value)
  (treemacs-file-follow-delay 0.2)
  (treemacs-file-name-transformer #'identity)
  (treemacs-find-workspace-method 'find-for-file-or-pick-first)
  (treemacs-git-command-pipe "")
  (treemacs-goto-tag-strategy 'refetch-index)
  (treemacs-header-scroll-indicators '(nil . "^^^^^^"))
  (treemacs-hide-dot-git-directory t)
  (treemacs-indentation 2)
  (treemacs-indentation-string " ")
  (treemacs-is-never-other-window nil)
  (treemacs-max-git-entries 5000)
  (treemacs-missing-project-action 'ask)
  (treemacs-move-files-by-mouse-dragging t)
  (treemacs-move-forward-on-expand nil)
  (treemacs-no-png-images nil)
  (treemacs-no-delete-other-windows t)
  (treemacs-project-follow-cleanup nil)
  ;; Store Treemacs's persistent state (e.g., collapsed directories) in a cache file.
  ;; Assumes `user-emacs-directory` is correctly set (e.g., to ~/.config/emacs/).
  (treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory))
  (treemacs-position 'left)
  (treemacs-read-string-input 'from-child-frame)
  (treemacs-recenter-distance 0.1)
  (treemacs-recenter-after-file-follow nil)
  (treemacs-recenter-after-tag-follow nil)
  (treemacs-recenter-after-project-jump 'always)
  (treemacs-recenter-after-project-expand 'on-distance)
  ;; Directories to ignore and litter from the tree.
  (treemacs-litter-directories '("/node_modules" "/.venv" "/.cask"))
  (treemacs-project-follow-into-home nil)
  (treemacs-show-cursor nil)
  (treemacs-show-hidden-files t)
  (treemacs-silent-filewatch nil)
  (treemacs-silent-refresh nil)
  (treemacs-sorting 'alphabetic-asc)
  (treemacs-select-when-already-in-treemacs 'move-back)
  (treemacs-space-between-root-nodes t)
  (treemacs-tag-follow-cleanup t)
  (treemacs-tag-follow-delay 1.5)
  (treemacs-text-scale nil)
  (treemacs-user-mode-line-format nil)
  (treemacs-user-header-line-format nil)
  (treemacs-wide-toggle-width 70)
  (treemacs-width 35)
  (treemacs-width-increment 1)
  (treemacs-width-is-initially-locked t)
  )

;; --- 3. `treemacs-projectile` (Integration with Projectile) ---
(use-package treemacs-projectile
  :straight t
  :after (treemacs projectile) ; Ensure `treemacs` and `projectile` are loaded first.
  )

;; --- 4. `treemacs-icons-dired` (Integration with Dired) ---
(use-package treemacs-icons-dired
  :straight t
  :hook (dired-mode . treemacs-icons-dired-enable-once) ; Enable icons in Dired once per Dired buffer.
  )

;; --- 5. `treemacs-all-the-icons` (Integration with All The Icons) ---
(use-package treemacs-all-the-icons
  :straight t
  :after (treemacs all-the-icons) ; Ensure both `treemacs` and `all-the-icons` are loaded.
  )

;; --- 6. `treemacs-magit` (Integration with Magit) ---
(use-package treemacs-magit
  :straight t
  :after (treemacs magit) ; Ensure both `treemacs` and `magit` are loaded.
  )

(provide 'init-projects)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-projects.el ends here
