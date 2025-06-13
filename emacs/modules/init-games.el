;;; init-games.el --- Emacs Games Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures various games available in Emacs, including Tetris,
;; Speed Type, 2048, and the classic Snow animation.

;;; Code:

;; --- 1. Tetris (Classic Block-Falling Game) ---
(use-package tetris
  :straight nil
  :ensure nil
  :commands (tetris)
  :bind
  ;; Custom keybindings for `tetris-mode-map` for a personalized experience.
  (:map tetris-mode-map
        ("C-p"   . tetris-rotate-prev)    ; Rotate counter-clockwise
        ("C-n"   . tetris-rotate-down)    ; Soft drop
        ("C-b"   . tetris-move-left)      ; Move left
        ("C-f"   . tetris-move-right)     ; Move right
        ("C-SPC" . tetris-move-bottom))   ; Hard drop
  :config
  ;; Advise `tetris-end-game` to save the window configuration.
  ;; This ensures Emacs's window layout is restored after a game.
  ;; `zap-scores` is a custom name for the advice, `activate` makes it active immediately.
  (defadvice tetris-end-game (around zap-scores activate)
    (save-window-excursion ad-do-it))
  )

;; --- 2. Speed Type (Typing Speed Test Game) ---
(use-package speed-type
  :straight t        ; Ensure straight.el manages this package.
  :commands (speed-type-text)) ; Only load when the `speed-type-text` command is called.

;; --- 3. 2048 Game (Number Puzzle Game) ---
(use-package 2048-game
  :straight t        ; Ensure straight.el manages this package.
  :commands (2048-game)) ; Only load when the `2048-game` command is called.

;; --- 4. Snow (Falling Snow Animation) ---
(use-package snow
  :straight t        ; Assuming 'snow' is available via straight.el.
                     ; If it's a local file not on MELPA, you'd need the :load-path.
  ;; :load-path (lambda () (expand-file-name "site-elisp/snow.el" user-emacs-directory))
  ;; The above :load-path is typically unnecessary if straight.el is used.
  :commands (snow))  ; Only load when the `snow` command is called.

(provide 'init-games)
;;; init-games.el ends here
