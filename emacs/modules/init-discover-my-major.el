;;; init-discover-my-major.el --- Discover Emacs mode information -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; This file configures and binds `discover-my-major`, a helpful utility
;; for understanding the active major and minor modes in a buffer.
;;
;;; Code:

(use-package discover-my-major
  :straight t ; Ensure straight.el manages this package.
  :defer t    ; Defer loading until the keybinding is used.
  :bind ("C-h C-m" . discover-my-major))

(provide 'init-discover-my-major)
;;; init-discover-my-major.el ends here
