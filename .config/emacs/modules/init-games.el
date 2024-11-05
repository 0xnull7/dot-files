;;; init-games.el --- -*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes tetris, speed-type, 2048
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'use-package))
;; TetrisConfig
(use-package tetris
  :ensure nil
  :commands (tetris)
  :bind
  (:map tetris-mode-map
        ("C-p" . tetris-rotate-prev)
        ("C-n" . tetris-rotate-down)
        ("C-b" . tetris-move-left)
        ("C-f" . tetris-move-right)
        ("C-SPC" . tetris-move-bottom))
  :config
  (defadvice tetris-end-game (around zap-scores activate)
    (save-window-excursion ad-do-it)))
;; -TetrisConfig

;; SpeedTypePac
(use-package speed-type
  :commands (speed-type-text))
;; -SpeedTypePac

;; 2048Pac
(use-package 2048-game
  :commands (2048-game))
;; -2048Pac

;; SnowPac
(use-package snow
  :load-path (lambda () (expand-file-name "site-elisp/snow.el" user-emacs-directory))
  :commands (snow))
;; -SnowPac

(provide 'init-games)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-games.el ends here
