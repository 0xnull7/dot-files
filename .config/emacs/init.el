;;; init.el --- -*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is the init.el file for My Emacs Config.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; BetterGC (Garbage Collection)
(defvar better-gc-cons-threshold 134217728 ; 128mb
  "The default value to use for `gc-cons-threshold'.

If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))
;; -BetterGC

;; AutoGC
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))
;; -AutoGC

;; LoadPath
(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))

(update-to-load-path (expand-file-name "modules" user-emacs-directory))
;; -LoadPath

;; Constants

(require 'init-const)

;; Packages

;; Package Management
(require 'init-package)

;; Global Functionalities
(require 'init-global-config)

(require 'init-func)

;; (require 'init-search)

;; (require 'init-crux)

;; (require 'init-avy)

;; (require 'init-winner)

(require 'init-undo-tree)

;; (require 'init-discover-my-major)

;; (require 'init-shell)

(require 'init-dired)

;; (require 'init-buffer)

;; UI Enhancements
(require 'init-ui-config)

(require 'init-scroll)

;; General Programming
(require 'init-magit)

(require 'init-syntax)

;; (require 'init-dumb-jump)

;; (require 'init-parens)

(require 'init-indent)

(require 'init-format)

(require 'init-comment)

(require 'init-edit)

(require 'init-header)

;; (require 'init-ein)

(require 'init-lsp)

(require 'init-company)

;; Programming
(require 'init-cc)

(require 'init-java)

(require 'init-python)

(require 'init-haskell)

(require 'init-latex)

(require 'init-buildsystem)

(require 'init-lua)

(require 'init-racket)

(require 'init-elisp)

(require 'init-markdown)

;; Web Development
(require 'init-webdev)

;; Office
(require 'init-org)

(require 'init-pdf)

;; Internet
(require 'init-leetcode)

;; (require 'init-eaf)

;; (require 'init-erc)

;; (require 'init-mu4e)

;; (require 'init-tramp)


;; (require 'init-debbugs)

;; (require 'init-eww)

;; Miscellaneous
(require 'init-misc)

(require 'init-games)

(require 'init-epaint)

(require 'init-zone)

;; (load "C:/Users/thebl/.emacs.d/custom-packages.el")
;; (load "C:/Users/thebl/.emacs.d/modules/latex-setup.el")
;; (load "C:/Users/thebl/.emacs.d/modules/prog-lang-setup.el")
;; (load "C:/Users/thebl/.emacs.d/modules/misc.el")
;; (load "C:/Users/thebl/.emacs.d/modules/word-wrap/autoload.el")
;; (load "C:/Users/thebl/.emacs.d/modules/word-wrap/config.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(+global-word-wrap-mode 1)
 '(LaTeX-indent-environment-list
   '(("document" current-indentation)))
 '(LaTeX-indent-level 4)
 '(LaTeX-item-indent 0)
 '(TeX-brace-indent-level 4)
 '(TeX-newline-function 'newline-and-indent)
 '(column-number-mode t)
 '(custom-enabled-themes '(monokai))
 '(custom-safe-themes
   '("2b20b4633721cc23869499012a69894293d49e147feeb833663fdc968f240873" "30d174000ea9cbddecd6cc695943afb7dba66b302a14f9db5dd65074e70cc744" "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "c7fd1708e08544d1df2cba59b85bd25263180b19b287489d4f17b9118487e718" "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" default))
 '(default-justification 'full)
 '(display-battery-mode t)
 '(display-fill-column-indicator t)
 '(display-fill-column-indicator-column t)
 '(display-time-mode t)
 '(electric-pair-mode t)
 '(electric-pair-pairs
   '((34 . 34)
     (8216 . 8217)
     (8220 . 8221)
     (40 . 41)
     (91 . 93)
     (123 . 125)
     (39 . 39)
     (60 . 62)))
 '(fill-column 150)
 '(fill-individual-varying-indent t)
 '(global-display-fill-column-indicator-mode nil)
 '(global-display-line-numbers-mode t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(visual-fill-column-adjust-for-text-scale t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:slant normal :weight bold :height 140 :width normal :family "JetBrains mono"))))
 '(hi-yellow ((t (:background "dim gray" :foreground "gold"))))
 '(highlight-thing ((t (:inherit 'hi-yellow)))))

;;; c-mode
(setq-default c-basic-offset 4
	      c-default-style '((java-mode . "java")
				(awk-mode . "awk")
				(other . "bsd")))

;; C#
(setq auto-mode-alist
      (append '(("\\.cs\\'" . csharp-mode))
              auto-mode-alist))

(provide 'init.el)
;;; init.el ends here.
