;;; init-buildsystem.el --- Modes for build systems and config files -*- lexical-binding: t; -*-

;;; Commentary:
;; Configures modes for Docker, Groovy (Jenkinsfiles), CMake, Bazel, and YAML.

;;; Code:

;; --- General Build System Modes ---
;; For each mode, we use the :mode keyword to ensure it's only loaded when
;; a relevant file is opened. This is more performant than a generic :defer.

(use-package dockerfile-mode
  :straight t
  :mode "Dockerfile\\'") ;; Regex needs to match the whole filename for exact match

(use-package groovy-mode
  :straight t
  :mode "\\.groovy\\'")

(use-package cmake-mode
  :straight t
  :mode "\\.cmake\\'")

(use-package bazel
  :straight t
  :mode ("\\.bzl\\'" . bazel-mode)
  ;; Add any specific bazel-mode configurations here if needed.
  ;; For instance:
  ;; :config
  ;; (add-hook 'bazel-mode-hook #'lsp-deferred) ; if using LSP for Bazel
  )

;; --- YAML Mode with Enhancements ---
(use-package yaml-mode
  :straight t ; Ensure `yaml-mode` is installed by straight.el
  :mode
  (("\\.yml\\'" . yaml-mode)
   ("\\.yaml\\'" . yaml-mode))
  :config
  (setq yaml-indent-offset 2)
  (setq yaml-tab-width 2)
  (setq yaml-indent-line-start t) ; Indent new lines
  (setq indent-tabs-mode nil) ; Prefer spaces

  ;; yaml-pro adds advanced editing features like moving subtrees.
  (use-package yaml-pro
    :straight t
    :hook (yaml-mode . yaml-pro-mode)
    :bind (:map yaml-pro-mode-map
                ("C-c M-p" . yaml-pro-move-subtree-up)
                ("C-c M-n" . yaml-pro-move-subtree-down))
    ;; Ensure yaml-pro is always loaded with yaml-mode for consistency
    :after yaml-mode
    )

  ;; Custom helper functions for working with YAML paths.
  ;; They are prefixed with `my/` to avoid namespace collisions.
  ;; Refined for robustness and clarity.

  (defun my/yaml--current-indentation-level ()
    "Return the current line's indentation level."
    (current-column)) ;; `current-column` is more direct than recursive `my/yaml--indentation-level`

  (defun my/yaml--clean-string (s)
    "Remove leading/trailing YAML specific characters from a string."
    (string-trim (replace-regexp-in-string "^[ -:]*\\(.*\\):?$" "\\1" s)))

  (defun my/yaml-path-at-point-string ()
    "Compute the YAML path at the current point as a string.
This function traverses up the YAML hierarchy to construct the full path."
    (save-excursion
      (let* ((original-line-str (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
             (original-level    (my/yaml--current-indentation-level))
             (path-components   '())
             (current-level     original-level)
             line-str prev-level)

        ;; Always add the current line's component if it's not empty
        (when (string-match-p "[^[:blank:]]" original-line-str)
          (setq path-components (list (my/yaml--clean-string original-line-str))))

        ;; Traverse upwards to find parent nodes
        (while (and (> (point) (point-min)) (> current-level 0))
          (setq prev-level current-level)
          (forward-line -1) ;; Move to previous line
          (setq line-str (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
          (setq current-level (my/yaml--current-indentation-level))

          (when (and (string-match-p "[^[:blank:]]" line-str) ;; Not an empty line
                     (< current-level prev-level))             ;; Is a parent node
            (setq path-components (cons (my/yaml--clean-string line-str) path-components))))

        (mapconcat 'identity path-components " › "))))

  (defun my/yaml-get-path-at-point ()
    "Display the YAML path at point in the echo area."
    (interactive)
    (message "Path: %s" (my/yaml-path-at-point-string)))

  ;; Bind the custom path function for easy access
  :bind (:map yaml-mode-map
              ("C-c C-p" . my/yaml-get-path-at-point))
  )

(provide 'init-buildsystem)
;;; init-buildsystem.el ends here
