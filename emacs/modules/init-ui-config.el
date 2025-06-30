;;; init-ui-config.el --- Enhanced User Interface Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures various UI-related packages and settings for a modern
;; and visually rich Emacs experience. It includes theme loading, icon support,
;; modeline configuration, and other visual and interaction enhancements.

;;; Code:

;; It's crucial that `package-initialize` has run or `straight.el` is fully
;; bootstrapped before any `use-package` form tries to load or configure packages.
;; In your main init.el, ensure straight.el's bootstrap is at the very top.

;; --- 1. Theme Loading (`doom-themes`) ---
;; Load doom-themes here. The :init hook ensures the theme is loaded early
;; after package manager initialization, preventing flicker.
(use-package doom-themes
  :straight t
  :ensure t
  :init
  ;; Load the theme immediately when doom-themes is available.
  ;; This ensures it's applied before other UI elements might be drawn, reducing flicker.
  (load-theme 'doom-one t) ; Load your desired Doom theme. 't' means no-confirm.
  :config
  ;; Configure Doom themes for various display elements
  (doom-themes-visual-bell-config) ; Configure visual bell (flashing screen instead of beep)
  (doom-themes-org-config)         ; Configure Org mode faces to match the theme
  ;; (doom-themes-neotree-config)   ; Uncomment if you use neotree
  )

;; --- 2. Basic Font Settings ---
;; Set the default font attributes. Adjust family, height, and weight as preferred.
;; 'JetBrains Mono' is an excellent choice for programming.
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
                    :slant 'normal
                    :weight 'bold
                    :height 160
                    :width 'normal)

;; --- 3. Icon Support (`nerd-icons`) ---
;; Nerd-icons is a modern icon font system with good performance and broad support.
(use-package nerd-icons
  :straight t
  :if (display-graphic-p) ; Only load if Emacs is running in a graphical environment
  )

;; --- 4. Cursor Highlighting (`beacon`) ---
;; Highlights the cursor briefly when switching windows or frames, helping you
;; quickly find where you are.
(use-package beacon
  :straight t
  :config
  (beacon-mode 1) ; Enable beacon-mode globally
  ;; Optional: Customize beacon appearance
  ;; (setq beacon-color "#5B96B5")
  ;; (setq beacon-blink-duration 0.2)
  )

;; --- 5. Startup Screen (`dashboard`) ---
;; Configures a custom startup screen with recent files, bookmarks, and projects.
;; Integrates with nerd-icons for a visual dashboard.
(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Welcome to Emacs!") ; Customize the banner title
  (setq dashboard-startup-banner 'official)
  (setq dashboard-center-content t) ; Center the content horizontally
  (setq dashboard-vertically-center-content t) ; Center the content vertically
  (setq dashboard-show-shortcuts t) ; Display shortcuts for dashboard items
  (setq dashboard-items '((recents . 10) ; Show 10 recent files
                          (bookmarks . 5) ; Show 5 bookmarks
                          (projects . 5))) ; Show 5 projects

  ;; Customize the shortcuts for dashboard items for quick navigation
  (setq dashboard-item-shortcuts '(("r" . "Open recent file")
                                  ("b" . "Jump to bookmark")
                                  ("p" . "Switch project")))

  ;; Use Nerd Icons for dashboard elements
  (setq dashboard-icon-type 'nerd-icons)
  ; (setq dashboard-display-icons-p t) ; Enable icon display in general
  (setq dashboard-set-file-icons t) ; Display icons next to file names

  ;; Optional: Force dashboard as the initial buffer.
  ;; This setting should be in your main init.el or early-init.el if you want
  ;; to guarantee it, as it affects initial buffer creation.
  ;; (setq initial-buffer-choice 'dashboard-open)
  )

;; --- 6. Mode Line Cleanup (`diminish`) ---
;; Used to hide or abbreviate minor mode indicators in the mode line,
;; reducing clutter.
(use-package diminish
  :straight t
  :config
  ;; Example: hide these minor modes if they're too verbose in your modeline
  (diminish 'electric-pair-mode)
  (diminish 'display-line-numbers-mode)
  (diminish 'prettify-symbols-mode)
  ;; Add more modes here as you fine-tune your modeline
  )

;; --- 7. Modern Mode Line (`doom-modeline`) ---
;; A highly customizable and beautiful mode line for Emacs.
(use-package doom-modeline
  :straight t
  ;; :hook (after-init . doom-modeline-mode) ; This is fine, but ':init' ensures it's active earlier
  :init
  ;; Activating it in `:init` ensures it's enabled as soon as the package is loaded,
  ;; which is usually early enough to prevent any default modeline from showing.
  (doom-modeline-mode 1)
  :config
  ;; General appearance and behavior
  (setq doom-modeline-height 25) ; Set modeline height
  (setq doom-modeline-bar-width 4) ; Set bar width
  (setq doom-modeline-hud nil) ; Disable HUD (often unnecessary)
  (setq doom-modeline-project-detection 'auto) ; Auto-detect projects based on common project files
  (setq doom-modeline-buffer-file-name-style 'auto) ; Auto-style buffer file names (e.g., relative, basename)

  ;; Icons and visual indicators (integrates with nerd-icons)
  (setq doom-modeline-icon t) ; Enable general icons
  (setq doom-modeline-major-mode-icon t) ; Enable major mode icons
  (setq doom-modeline-major-mode-color-icon t) ; Enable colored major mode icons
  (setq doom-modeline-buffer-state-icon t) ; Enable buffer state icons (modified/read-only)
  (setq doom-modeline-buffer-modification-icon t) ; Enable buffer modification icons
  (setq doom-modeline-lsp-icon t) ; Enable LSP icons

  ;; Time and Battery (Doom modeline manages these, so original display-time-mode/display-battery-mode are redundant)
  (setq doom-modeline-time t) ; Enable time display
  (setq doom-modeline-time-icon t) ; Enable time icon
  (setq doom-modeline-time-live-icon t) ; Enable live time icon
  (setq doom-modeline-time-analogue-clock nil) ; Disable analogue clock (often distracting)
  (setq doom-modeline-battery t) ; Enable battery display (only if battery-status-function is available)

  ;; Buffer information display
  (setq doom-modeline-buffer-name t) ; Enable buffer name display
  (setq doom-modeline-highlight-modified-buffer-name t) ; Highlight modified buffer names

  ;; Position and line numbers
  ;; Doom modeline usually handles these, making global-display-line-numbers-mode or column-number-mode redundant *in the modeline*.
  ;; Keep `global-display-line-numbers-mode` in init.el if you want them *in the buffer*.
  (setq doom-modeline-column-zero-based nil) ; Use 1-based column numbers (more common)
  (setq doom-modeline-percent-position '(-3 "%p")) ; Position percentage format
  (setq doom-modeline-position-line-format '("L%l")) ; Line number format
  (setq doom-modeline-position-column-format '("C%c")) ; Column number format
  (setq doom-modeline-position-column-line-format '("%l:%c")) ; Line:Column format

  ;; Other segments and advanced settings (keep configuring as needed)
  (setq doom-modeline-minor-modes nil) ; Disable minor modes display (can be too verbose)
  (setq doom-modeline-enable-word-count nil) ; Disable global word count
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode)) ; Enable in specific modes
  (setq doom-modeline-buffer-encoding t) ; Enable buffer encoding display
  (setq doom-modeline-indent-info nil) ; Disable indent info (often redundant with other indicators)
  (setq doom-modeline-total-line-number nil) ; Disable total line number display (redundant with percent position)
  (setq doom-modeline-vcs-icon t) ; Enable VCS icons
  (setq doom-modeline-vcs-max-length 15) ; Set VCS max length
  (setq doom-modeline-vcs-display-function #'doom-modeline-vcs-name) ; Set VCS display function
  (setq doom-modeline-check-icon t) ; Enable check icons (e.g., flycheck errors/warnings)
  (setq doom-modeline-check-simple-format nil) ; Disable simple check format
  (setq doom-modeline-number-limit 99) ; Set number limit
  (setq doom-modeline-workspace-name t) ; Enable workspace name display
  (setq doom-modeline-persp-name t) ; Enable perspective name display
  (setq doom-modeline-display-default-persp-name nil) ; Disable default perspective name display
  (setq doom-modeline-persp-icon t) ; Enable perspective icons
  (setq doom-modeline-github nil) ; Disable GitHub display (unless you specifically need it)
  (setq doom-modeline-modal t) ; Enable modal display (e.g., showing current state like INSERT, NORMAL)
  (setq doom-modeline-modal-icon t) ; Enable modal icons
  (setq doom-modeline-modal-modern-icon t) ; Enable modern modal icons
  (setq doom-modeline-always-show-macro-register nil) ; Disable macro register display
  (setq doom-modeline-mu4e nil) ; Disable mu4e display (unless using mu4e)
  (setq doom-modeline-gnus t) ; Enable Gnus display (if using Gnus)
  (setq doom-modeline-irc t) ; Enable IRC display (if using ERC)
  (setq doom-modeline-display-misc-in-all-mode-lines t) ; Display misc in all mode lines
  (setq doom-modeline-buffer-file-name-function #'identity) ; Use identity for filename
  (setq doom-modeline-buffer-file-truename-function #'identity) ; Use identity for truename
  (setq doom-modeline-env-version t) ; Enable environment version display (e.g., Python venv)
  (setq doom-modeline-env-enable-python t) ; Enable Python env display
  (setq doom-modeline-env-enable-ruby t) ; Enable Ruby env display
  (setq doom-modeline-env-enable-perl t) ; Enable Perl env display
  (setq doom-modeline-env-enable-go t) ; Enable Go env display
  (setq doom-modeline-env-enable-elixir t) ; Enable Elixir env display
  (setq doom-modeline-env-enable-rust t) ; Enable Rust env display
  (setq doom-modeline-env-python-executable "python") ; Python executable path
  (setq doom-modeline-env-ruby-executable "ruby") ; Ruby executable path
  (setq doom-modeline-env-perl-executable "perl") ; Perl executable path
  (setq doom-modeline-env-go-executable "go") ; Go executable path
  (setq doom-modeline-env-elixir-executable "iex") ; Elixir executable path
  (setq doom-modeline-env-rust-executable "rustc") ; Rust executable path
  (setq doom-modeline-env-load-string "...") ; Environment load string
  (setq doom-modeline-always-visible-segments '(mu4e irc)) ; Set always visible segments
  (setq doom-modeline-before-update-env-hook nil) ; Disable before update env hook
  (setq doom-modeline-after-update-env-hook nil) ; Disable after update env hook
  )

;; --- 8. Emoji Support (`emojify`) ---
;; Displays emoji characters using appropriate fonts.
(use-package emojify
  :straight t
  :hook (after-init . global-emojify-mode) ; Enable globally after init
  )

;; --- 9. Custom Scrollbars (`yascroll`) ---
;; Provides custom, more visually appealing scrollbars.
(use-package yascroll
  :straight t
  :config
  (global-yascroll-bar-mode 1) ; Enable custom scrollbars globally
  ;; Optional: Further customize scrollbar appearance
  ;; (setq yascroll:width 8)
  ;; (setq yascroll:scroll-bar-background-color "#555")
  )

;; --- 10. Highlight Current Line (`global-hl-line-mode`) ---
;; Highlights the current line in every buffer.
(global-hl-line-mode 1)

;; --- 11. `prettify-symbols-mode` (Symbol Replacement) ---
;; Replaces common text sequences with pretty Unicode symbols (e.g., "lambda" to "λ").
(add-hook 'after-init-hook 'global-prettify-symbols-mode)

;; Define a function to customize the 'prettify-symbols-alist'.
(defun my-custom-prettify-symbols ()
  "Make some word or string show as pretty Unicode symbols.
  See https://unicodelookup.com for more Unicode symbols."
  (setq prettify-symbols-alist
        '(("lambda" . ?λ)        ; Replace "lambda" with the Greek lambda symbol
          ("delta" . ?δ)         ; Replace "delta" with the Greek delta symbol
          ("epsilon" . ?ε)       ; Replace "epsilon" with the Greek epsilon symbol
          ("->" . ?→)            ; Replace "->" with a right arrow symbol
          ("<=" . ?≤)            ; Replace "<=" with a less-than-or-equal-to symbol
          (">=" . ?≥)            ; Replace ">=" with a greater-than-or-equal-to symbol
          ("=>" . ?⇒)            ; Double arrow
          ("!=" . ?≠)            ; Not equal
          ("inf" . ?∞)           ; Infinity
          ("forall" . ?∀)        ; For all
          ("exists" . ?∃)        ; There exists
          )))

;; Apply the custom symbol list in programming modes and Org mode.
(add-hook 'prog-mode-hook 'my-custom-prettify-symbols)
(add-hook 'org-mode-hook 'my-custom-prettify-symbols)

;; --- 12. Custom Frame Title Bar ---
;; Sets the title bar of Emacs frames to show "EMACS - username@hostname - buffername".
(setq-default frame-title-format '("EMACS - " user-login-name "@" system-name " - %b"))

;; --- 13. `y-or-n-p` (Consistent Yes/No Prompts) ---
;; Makes `yes-or-no-p` prompts use single 'y' or 'n' characters instead of full words,
;; and disables graphical dialog boxes for prompts.
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

;; --- 14. Startup Screen Settings ---
;; Prevents the default Emacs splash screen from appearing.
;; This line is important and should ideally be in early-init.el or very early in init.el.
(setq inhibit-startup-screen t)
;; (setq initial-major-mode 'text-mode) ; Uncomment to start in text mode

;; Optional: Directly set initial buffer to dashboard.
;; This is a strong way to ensure the dashboard loads.
;; Consider moving this to your main init.el or early-init.el
;; if you want it to be applied universally.
;; (setq initial-buffer-choice 'dashboard-open)

;; --- 15. Line and Column Numbers ---
;; Enable line numbers and column numbers.
;; `global-display-line-numbers-mode` is handled in init.el.
;; `doom-modeline` also shows column numbers, making `column-number-mode`
;; redundant *if* you just want it in the modeline.
;; If you want column number highlighted in the buffer body, keep `column-number-mode`
;; enabled from your `init.el` or add a hook here.
(add-hook 'text-mode-hook #'display-line-numbers-mode) ; Enable line numbers for text modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode) ; Enable line numbers for programming modes

;; --- 16. Pixel-Precise Scrolling (Emacs 29.1+) ---
;; Enables smoother, pixel-by-pixel scrolling in newer Emacs versions.
(when (version<= "29.1" emacs-version)
  (pixel-scroll-precision-mode 1))

(provide 'init-ui-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui-config.el ends here
