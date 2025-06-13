;;; init-scroll.el --- Smooth Scrolling Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file customizes Emacs's scrolling behavior for a smoother experience,
;; adjusting vertical and horizontal scroll parameters, and mouse wheel behavior.

;;; Code:

;; --- 1. Vertical Scrolling ---
;; Set the number of lines to scroll when the cursor moves off-screen.
;; A value of 1 ensures very fine-grained, smooth scrolling.
(setq scroll-step 1)

;; Keep at least N lines of text around the cursor when scrolling.
;; A margin of 1 means the cursor always tries to stay 1 line away from the edge.
(setq scroll-margin 1)

;; Scroll conservatively, meaning Emacs will try to keep the cursor within
;; the current window as much as possible before scrolling.
;; A value > 100 means scroll only when the cursor moves off-screen.
(setq scroll-conservatively 101)

;; How aggressively to scroll up when the cursor moves up.
;; A small value like 0.01 means it scrolls very gradually.
(setq scroll-up-aggressively 0.01)

;; How aggressively to scroll down when the cursor moves down.
;; A small value like 0.01 means it scrolls very gradually.
(setq scroll-down-aggressively 0.01)

;; Do not automatically scroll the window vertically if the cursor moves off-screen.
;; This allows `scroll-margin` and `scroll-conservatively` to handle it.
(setq auto-window-vscroll nil)

;; Set to nil for precise, step-by-step scrolling instead of larger jumps.
(setq fast-but-imprecise-scrolling nil)

;; Control mouse wheel scrolling behavior:
;; `(1 ((shift) . 1))` means 1 line per mouse wheel tick, and 1 line per tick with Shift.
;; You might prefer a larger number for faster scrolling, e.g., `(3 ((shift) . 1))`.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Disable progressive speed for mouse wheel scrolling.
;; This means each tick scrolls the same amount, regardless of how fast you scroll.
(setq mouse-wheel-progressive-speed nil)


;; --- 2. Horizontal Scrolling ---
;; Set the number of columns to scroll horizontally.
(setq hscroll-step 1)

;; Keep at least N columns of text around the cursor when scrolling horizontally.
(setq hscroll-margin 1)

(provide 'init-scroll)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-scroll.el ends here
