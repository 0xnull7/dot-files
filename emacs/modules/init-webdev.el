;;; init-webdev.el --- Web Development Environment Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file configures Emacs for web development, including modes for HTML, CSS,
;; JavaScript, TypeScript, and integrations for LSP, Emmet, and code beautification.

;;; Code:

;; --- 1. `web-mode` (HTML, CSS, JS, and templating) ---
(use-package web-mode
  :straight t
  :mode
  (("\\.html?\\'" . web-mode)    ; HTML files (.html, .htm)
   ("\\.css?\\'" . web-mode)     ; CSS files (.css)
   ("\\.js\\'" . web-mode)       ; JavaScript files (.js)
   ("\\.ts\\'" . web-mode)       ; TypeScript files (.ts)
   ("\\.phtml\\'" . web-mode)    ; PHP HTML templates
   ("\\.tpl\\.php\\'" . web-mode) ; Smarty PHP templates
   ("\\.[agj]sp\\'" . web-mode)  ; ASP, JSP
   ("\\.as[cp]x\\'" . web-mode)  ; ASP.NET
   ("\\.erb\\'" . web-mode)      ; Embedded Ruby (Rails)
   ("\\.mustache\\'" . web-mode) ; Mustache templates
   ("\\.djhtml\\'" . web-mode)   ; Django HTML templates
   ("\\.[t]?html?\\'" . web-mode)) ; Generic HTML/templating (e.g., .tpl.html)

  :custom-face
  ;; Customize faces for `web-mode` for better aesthetics.
  (css-selector ((t (:inherit default :foreground "#66CCFF")))) ; Blue for CSS selectors.
  (font-lock-comment-face ((t (:foreground "#828282"))))       ; Dim gray for comments.

  :custom
  ;; Define extra auto-pairing rules for templating languages.
  (web-mode-extra-auto-pairs
   '(("erb" . (("beg" "end")))
     ("php" . (("beg" "end") ("beg" "end"))))) ; Redundant second "beg" "end" for php, removed for clarity.
  (web-mode-enable-auto-pairing t)         ; Enable automatic pairing of brackets/quotes.
  (web-mode-enable-css-colorization t)     ; Enable CSS color highlighting.
  (web-mode-enable-current-column-highlight t) ; Highlight current column.
  (web-mode-enable-current-element-highlight t) ; Highlight current HTML/XML element.
  (web-mode-exclude-excluded-mime-types '("text/x-c#")) ; Exclude C# from `web-mode` MIME types.

  :config
  ;; Set up `company-mode` backends for `web-mode` for completion.
  (setq-local company-backends '(company-css company-web-html company-yasnippet company-files))
  ;; Enable `emmet-mode` automatically when `web-mode` is active.
  (add-hook 'web-mode-hook 'emmet-mode)
  )

;; --- 2. `js2-mode` (Advanced JavaScript Mode) ---
(use-package js2-mode
  :straight t
  :mode "\\.js\\'"
  :interpreter "node"
  :bind
  ;; Unbind `M-.` in `js-mode-map` if it conflicts with other packages (e.g., `lsp-mode`'s `lsp-find-definition`).
  (:map js2-mode-map ("M-." . nil))
  )

;; --- 3. `typescript-mode` (TypeScript Mode) ---
(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'"
  :commands (typescript-mode)
  )

;; --- 4. `tide` (TypeScript Interactive Development Environment) ---
(use-package tide
  :straight t
  :after (typescript-mode company flycheck)
  :hook
  ((typescript-mode . tide-setup)
   (typescript-mode . tide-hl-identifier-mode)  ; Highlight identifiers.
   (before-save . tide-format-before-save))     ; Format TypeScript code before saving.
  :config
  ;; Ensure `typescript-language-server` is in your PATH, or set `tide-tsserver-executable`.
  ;; (setq tide-tsserver-executable "/usr/local/bin/typescript-language-server")
  )

;; --- 5. `web-beautify` (Code Formatter) ---
(use-package web-beautify
  :straight t
  :init
  ;; Hook up beautification functions to `before-save-hook` for various modes.
  ;; Using `eval-after-load` ensures hooks are added only when the major mode is loaded.
  (add-hook 'js2-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'web-beautify-js-buffer nil 'local))) ; `local` ensures hook is buffer-local.
  (add-hook 'json-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'web-beautify-js-buffer nil 'local)))
  (add-hook 'html-mode-hook ; `sgml-mode` is often the parent, but `html-mode-hook` is more direct.
            (lambda ()
              (add-hook 'before-save-hook 'web-beautify-html-buffer nil 'local)))
  (add-hook 'web-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'web-beautify-html-buffer nil 'local))) ; Can also use `web-beautify-js-buffer` or `web-beautify-css-buffer` if context allows.
  (add-hook 'css-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'web-beautify-css-buffer nil 'local)))
  ;; You might need to install external beautifier executables (e.g., `npm install -g js-beautify`).
  )

;; --- 6. `emmet-mode` (HTML/CSS Shorthand Expansion) ---
(use-package emmet-mode
  :straight t
  :hook
  ((web-mode . emmet-mode)  ; Enable in `web-mode`.
   (css-mode . emmet-mode)  ; Enable in `css-mode`.
   (sgml-mode . emmet-mode)) ; Enable in `sgml-mode` (often parent of HTML/XML modes).
  :custom
  (emmet-move-cursor-between-quotes t) ; Move cursor between quotes after expansion.
  ;; Add `rjsx-mode` (if used) to Emmet's JSX major modes for proper expansion.
  (emmet-jsx-major-modes '(rjsx-mode))
  :config
  ;; Consider adding keybindings for `emmet-expand-line` or other commands.
  ;; (define-key emmet-mode-map (kbd "C-j") 'emmet-expand-line)
  )

;; --- 7. `instant-rename-tag` (HTML/XML Tag Renaming) ---
(use-package instant-rename-tag
  :load-path (lambda () (expand-file-name "site-elisp/instant-rename-tag" user-emacs-directory))
  :bind ("C-z <" . instant-rename-tag) ; Bind `C-z <` to rename tags.
  )

;; --- 8. `json-mode` (JSON Editing) ---
(use-package json-mode
  :straight t
  :mode "\\.json\\'"
  )

;; --- Optional: `rjsx-mode` (React JSX/TSX) ---
(use-package rjsx-mode
  :straight t
  :mode ("\\.jsx\\'" "\\.tsx\\'")
  :hook ((rjsx-mode . tide-setup) ; If using Tide with TSX
         (rjsx-mode . web-beautify-js-buffer)) ; Format JSX
  :config
  (electric-pair-mode)
  )

;; --- Optional: `vue-mode` (Vue.js Single File Components) ---
;; Mode for `.vue` files.
;; (use-package vue-mode
;;   :straight t
;;   :mode "\\.vue\\'"
;;   :commands (vue-mode)
;;   :hook ((vue-mode . tide-setup)) ; If you have TypeScript in your Vue SFCs
;;   )

(provide 'init-webdev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-webdev.el ends here
