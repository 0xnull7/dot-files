;;; prog-lang-setup.el --- Programming Language Setup

;;; Commentary:
;; This file have all the needed to start programming.



;;; Code:

;; Ensure use-package is available
(eval-when-compile
  (require 'use-package))


;; LSP mode
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((c-mode . lsp)
	 (c++-mode . lsp)
	 (csharp-mode . lsp)
	 (LaTeX-mode . lsp)
	 (python-mode . lsp)
	 (java-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-idle-delay 0.1)
  (setq lsp-tex-server 'texlab))

;; LSP UI enhancements
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; Debugger
;; (use-package dap-mode)

;; Linter
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :hook ((c-mode . flycheck-mode)
	 (c++-mode . flycheck-mode)
	 (csharp-mode . flycheck-mode)
	 (LaTeX-mode . flycheck-mode)
	 (python-mode . flycheck-mode)
	 (java-mode . flycheck-mode))
  :config
  (setq flycheck-display-errors-function
	#'flycheck-display-error-messages-unless-error-list)

  (setq flycheck-indication-mode nil))


;; AUCTeX - enhanced LaTeX mode
(use-package auctex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-engine 'luatex)
  (setq TeX-command-default "LuaLaTeX")
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t))

;; C/C++
;; FOR SETTING UP BUILD SYSTEMS CHECK THIS LINK -> https://fanpengkong.com/post/emacs-ccpp/emacs-ccpp/
;; ALSO CHECK THIS LINK -> https://martinsosic.com/development/emacs/2017/12/09/emacs-cpp-ide.html

;; when you press RET, the curly braces automatically add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))
(use-package modern-cpp-font-lock
  :ensure t)

(use-package srefactor
  :ensure t
  :config
  (semantic-mode 1)
  (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
  (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
  )

;; C#

;; CDLaTeX - fast LaTeX insertion
(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . turn-on-cdlatex)
  :config
  (setq cdlatex-math-symbol-alist '((?^ ("^{}" "\\hat{}"))))
  (setq cdlatex-math-modify-alist '((116 "\\text" nil t))))

;; Company-AUCTeX backend
(use-package company-auctex
  :ensure t
  :after (company auctex)
  :config
  (company-auctex-init))

;; Company-Math backend
(use-package company-math
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-math-symbols-latex))


;; Elisp
(use-package elisp-autofmt
  :commands (elisp-autofmt-mode elisp-autofmt-buffer)
  :hook (emacs-lisp-mode . elisp-autofmt-mode))

;; Java
;; THIS IS A GOOD TUTORIAL FOR STARTING -> https://xpressrazor.wordpress.com/2020/11/04/java-programming-in-emacs/

(use-package lsp-java
  :ensure t
  :hook (java-mode . lsp))

;; Lua
(use-package lua-mode
  :ensure t)

;; Mardown
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package markdown-toc
  :ensure t)

(use-package markdownfmt
  :ensure t
  :hook (markdown-mode . markdownfmt-enable-on-save))

;; Rust
(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save nil)
  :custom
  (rustic-cargo-use-last-stored-arguments t)
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;; Standard Meta Language (ml)
;; (use-package sml)

;; Web-Mode
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  (setq emmet-move-cursor-between-quotes t) ;; default nil
  (add-to-list 'emmet-jsx-major-modes 'rjsx-mode))

(use-package js2-mode
  :ensure t)

(use-package json-mode
  :ensure t
  :defer t)

;; (use-package rjsx-mode)

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package web-beautify
  :ensure t
  :config
  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook
               (lambda ()
		 (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

  (eval-after-load 'json-mode
    '(add-hook 'json-mode-hook
               (lambda ()
		 (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))

  (eval-after-load 'sgml-mode
    '(add-hook 'html-mode-hook
               (lambda ()
		 (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

  (eval-after-load 'web-mode
    '(add-hook 'web-mode-hook
               (lambda ()
		 (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))

  (eval-after-load 'css-mode
    '(add-hook 'css-mode-hook
               (lambda ()
		 (add-hook 'before-save-hook 'web-beautify-css-buffer t t)))))

(use-package web-mode
  :ensure t
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.css?\\'" . web-mode)
   ("\\.js\\'" . web-mode)
   ("\\.ts\\'" . web-mode)
   ("\\.phtml\\'" . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode))
  :config
  (setq web-mode-extra-auto-pairs
	'(("erb"  . (("beg" "end")))
          ("php"  . (("beg" "end")
                     ("beg" "end")))
	  ))
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
  (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files))
  (add-hook 'web-mode-hook  'emmet-mode)
  )



(provide 'prog-lang-setup)
;;; prog-lang-setup.el ends here
