
(load "~/.config/emacs/custom-packages.el")
(load "~/.config/emacs/modules/latex-setup.el")
(load "~/.config/emacs/modules/prog-lang-setup.el")
(load "~/.config/emacs/modules/misc.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes '(doom-dark+))
 '(display-battery-mode t)
 '(display-time-mode t)
 '(global-display-line-numbers-mode t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:slant normal :weight bold :height 165 :width normal :family "Fira Code")))))

;;; c-mode
(setq-default c-basic-offset 4
              c-default-style '((java-mode . "java")
                                (awk-mode . "awk")
                                (other . "bsd")))

;; (defun electric-pair ()
;;   "If at end of line, insert character pair without surrounding spaces.
;;     Otherwise, just insert the typed character."
;;   (interactive)
;;   (if (eolp) (let (parens-require-spaces) (insert-pair)) (self-insert-command 1)))

;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (define-key prog-mode-map "\"" 'electric-pair)
;;             (define-key prog-mode-map "\'" 'electric-pair)
;;             (define-key prog-mode-map "(" 'electric-pair)
;; 	    (define-key prog-mode-map "[" 'electric-pair)
;;             (define-key prog-mode-map "{" 'electric-pair)))

