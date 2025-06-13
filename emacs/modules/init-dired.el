;;; init-dired.el --- Configuration for the Dired file manager -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; This file enhances the built-in Dired file manager with quality-of-life
;; improvements and better visual integration.
;;
;;; Code:

;; --- 1. Dired Core Configuration ---
(use-package dired
  :straight nil
  :ensure nil
  :bind (("C-x C-j" . dired-jump))
  :config
  (setq
   dired-listing-switches "-lah" ; Show human-readable sizes and all files.
   dired-recursive-deletes 'always ; Always confirm recursive deletes.
   dired-recursive-copies 'always ; Always confirm recursive copies.
   dired-dwim-target t            ; "Do What I Mean" for copy/rename targets.
   delete-by-moving-to-trash t    ; Move deleted files to trash instead of permanent deletion.
   load-prefer-newer t)           ; Prefer newer versions of files when loading.

  :hook
  (dired-mode .
              (lambda ()
                ;; Remap RET to 'dired-find-alternate-file' in Dired buffers.
                ;; This opens files/directories in the current window.
                (local-set-key (kbd "RET") #'dired-find-alternate-file)
                ;; Remap 'h' to quickly jump to the parent directory.
                (local-set-key (kbd "h")
                               (lambda ()
                                 (interactive)
                                 (find-alternate-file ".."))))))

;; --- 2. Dired Enhancements ---

;; `all-the-icons-dired`: Adds icons to Dired listings for better visual scanning.
(use-package all-the-icons-dired
  :straight t
  :hook (dired-mode . all-the-icons-dired-mode)) ; Activate the minor mode in Dired buffers.

;; `disk-usage`: Provides a visual disk usage tool within Emacs.
(use-package disk-usage
  :straight t
  :commands (disk-usage)) ; Only load the package when 'disk-usage' command is called.

(provide 'init-dired)

;;; init-dired.el ends here
