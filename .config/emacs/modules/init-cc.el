;;; init-cc.el --- -*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initialize ccls modern-cpp-font-lock go-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'use-package))

;; C/C++
;; FOR SETTING UP BUILD SYSTEMS CHECK THIS LINK -> https://fanpengkong.com/post/emacs-ccpp/emacs-ccpp/
;; ALSO CHECK THIS LINK -> https://martinsosic.com/development/emacs/2017/12/09/emacs-cpp-ide.html

;; CCLSPac
(use-package ccls
  :defer t
  :if (not *sys/win32*)
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  :custom
  (ccls-executable (executable-find "ccls")) ; Add ccls to path if you haven't done so
  (ccls-sem-highlight-method 'font-lock)
  (ccls-enable-skipped-ranges nil)
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection (cons ccls-executable ccls-args))
    :major-modes '(c-mode c++-mode cuda-mode objc-mode)
    :server-id 'ccls-remote
    :multi-root nil
    :remote? t
    :notification-handlers
    (lsp-ht ("$ccls/publishSkippedRanges" #'ccls--publish-skipped-ranges)
            ("$ccls/publishSemanticHighlight" #'ccls--publish-semantic-highlight))
    :initialization-options (lambda () ccls-initialization-options)
    :library-folders-fn nil)))
;; -CCLSPac

;; CPPFontLockPac
(use-package modern-cpp-font-lock
  :diminish t
  :init (modern-c++-font-lock-global-mode t))
;; -CPPFontLockPac

;; (use-package srefactor
;;   :ensure t
;;   :config
;;   (semantic-mode 1)
;;   (define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
;;   (define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
;;   )

;; GoPac
(use-package go-mode
  :mode "\\.go\\'"
  :hook (before-save . gofmt-before-save)
  :custom (gofmt-command "goimports"))
;; -GoPac

(use-package rustic
  :ensure t
  :mode "\\.rs\\'"
  :bind (:map rust-mode-map ("C-c C-c" . rust-run))
  :custom
  (rust-format-on-save t)
  (rustic-cargo-use-last-stored-arguments t)
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  :config
  (use-package flycheck-rust
    :after flycheck
    :config
    (with-eval-after-load 'rust-mode
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

;; RustPac
;; (use-package rust-mode
;;   :mode "\\.rs\\'"
;;   :custom
;;   (rust-format-on-save t)
;;   :bind (:map rust-mode-map ("C-c C-c" . rust-run))
;;   :config
;;   )
;; -RustPac

(provide 'init-cc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-cc.el ends here
