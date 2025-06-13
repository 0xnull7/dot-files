;;; init-crux.el --- Configuration for Crux, a Collection of Ridiculously Useful eXtensions -*- lexical-binding: t; -*-

;;; Commentary:
;; Crux provides a suite of useful interactive commands that enhance editing and
;; window management.

;;; Code:

(use-package crux
  :straight t
  :defer t ; Defer loading until a command is called.
  :bind (("C-a" . crux-move-beginning-of-line)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-x K" . crux-kill-other-buffers)
         ;; crux-smart-kill is often more intuitive than kill-line.
         ("C-k" . crux-smart-kill-line))
  :config
  ;; Define several commands that can operate on the region or whole buffer.
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)

  ;; Provide a convenient alias for a very common operation.
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(provide 'init-crux)
;;; init-crux.el ends here
