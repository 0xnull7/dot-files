;;; init-buffer.el --- Window, Buffer, and Tab Management -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures packages related to managing Emacs windows, buffers, and tabs.
;;
;; Code:

;; --- 1. Ace Window (quickly switch windows) ---
(use-package ace-window
  :straight t ; Use :straight instead of :ensure for consistency with straight.el
  :bind ("M-o" . ace-window)
  :config
  ;; No specific configuration needed beyond the binding.
  ;; Add any specific customizations here if required, e.g.,
  ;; (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )

;; --- 2. Buffer Move (move buffers between windows) ---
(use-package buffer-move
  :straight t
  :bind (("<M-S-up>"    . buf-move-up)
         ("<M-S-down>"  . buf-move-down)
         ("<M-S-left>"  . buf-move-left)
         ("<M-S-right>" . buf-move-right)))

;; --- 3. Buffer Name Relative (show relative paths in buffer names) ---
(use-package buffer-name-relative
  :straight t
  :config
  (buffer-name-relative-mode 1)) ; Enable the minor mode

;; --- 4. Centaur Tabs (tab bar interface) ---
(use-package centaur-tabs
  :straight t
  :init
  ;; Initialize key bindings and enable the mode globally.
  ;; `centaur-tabs-mode` in `:config` is usually sufficient for global activation.
  (setq centaur-tabs-enable-key-bindings t)

  :config
  (centaur-tabs-mode t) ;; Activate the global minor mode.

  ;; General appearance and behavior settings.
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 32
        centaur-tabs-set-icons t
        centaur-tabs-show-new-tab-button t
        centaur-tabs-set-modified-marker t
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-set-bar 'left
        centaur-tabs-show-count t
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-close-button ""
        centaur-tabs-modified-marker ""
        x-underline-at-descent-line t
        centaur-tabs-left-edge-margin nil)

  ;; Font settings. Applying `centaur-tabs-change-fonts` twice is redundant
  ;; and can lead to unexpected results. Choose one primary font.
  ;; Using `face-attribute 'default :font` is usually the most robust.
  (centaur-tabs-change-fonts (face-attribute 'default :font) 110)
  ;; (centaur-tabs-change-fonts "Fira Sans Bold" 120) ; Commented out, choose one or remove

  ;; Buffer naming uniqueness for tabs.
  ;; `uniquify-separator` and `uniquify-buffer-name-style` are global settings
  ;; that affect all unique buffer naming, not just Centaur Tabs.
  ;; It's fine to set them here, but be aware of their global impact.
  (setq uniquify-separator "/"
        uniquify-buffer-name-style 'forward)

  ;; Other useful Centaur Tabs settings (uncomment to enable):
  ;; (centaur-tabs-enable-buffer-alphabetical-reordering) ; For automatic tab reordering
  ;; (setq centaur-tabs-adjust-buffer-order t)            ; Works with reordering
  (centaur-tabs-headline-match) ; Apply headline matching for tabs (if using org-mode)

  :bind
  ;; Bindings should be within the :bind section.
  ("C-<prior>"   . centaur-tabs-backward)
  ("C-<next>"    . centaur-tabs-forward)
  ("C-S-<prior>" . centaur-tabs-move-current-tab-to-left)
  ("C-S-<next>"  . centaur-tabs-move-current-tab-to-right))

;; --- 5. Winum (numbering windows) ---
(use-package winum
  :straight t
  :config
  (winum-mode t)
  ;; Consider adding bindings for winum-select-window-N, e.g.:
  ;; (define-key global-map (kbd "M-1") 'winum-select-window-1)
  ;; (define-key global-map (kbd "M-2") 'winum-select-window-2)
  )

;; --- 6. Ibuffer (built-in buffer list) ---
(use-package ibuffer
  :straight nil ; This is correct for built-in packages.
  :bind ("C-x C-b" . ibuffer)
  :config
  ;; A detailed format for ibuffer, providing more context at a glance.
  ;; This is a very good, comprehensive format.
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (name 35 35 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename))))

;; --- 7. Ibuffer-VC (grouping by version control root) ---
(use-package ibuffer-vc
  :straight t
  ;; Hook into ibuffer to enable grouping by version control project root.
  ;; This hook is effective and will set up the filter groups when ibuffer-mode starts.
  :hook (ibuffer-mode . ibuffer-vc-set-filter-groups-by-vc-root)
  :config
  (setq ibuffer-vc-skip-if-remote nil))

(provide 'init-buffer)
;;; init-buffer.el ends here
