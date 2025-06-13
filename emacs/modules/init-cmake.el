;;; init-cmake.el --- CMake Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures Emacs for editing CMake files (`CMakeLists.txt`),
;; using `cmake-mode` for syntax highlighting and basic features.

;;; Code:

;; --- 1. `cmake-mode` (Major Mode for CMake) ---
(use-package cmake-mode
  :straight t
  :mode "CMakeLists\\.txt\\'"
  :hook (cmake-mode . (lambda () (setq-local tab-width 4)))
  :config
  (setq cmake-tab-width 4)
  )

(provide 'init-cmake)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-cmake.el ends here
