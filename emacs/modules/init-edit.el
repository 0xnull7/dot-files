;;; init-edit.el --- -*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes iedit, awesome-pair, delete-block
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(eval-when-compile
  (require 'init-global-config)
  (require 'use-package))

;; IEditPac
(use-package iedit
  :bind ("C-z ," . iedit-mode)
  :diminish)
;; -IEditPac

;; AwesomePairPac
; (use-package awesome-pair
;   :straight nil
;   :load-path (lambda () (expand-file-name "site-elisp/awesome-pair" user-emacs-directory))
;   :bind
;   (:map prog-mode-map
;         (("M-D" . awesome-pair-kill)
;          ("SPC" . awesome-pair-space)
;          ("=" . awesome-pair-equal)
;          ("M-F" . awesome-pair-jump-right)
;          ("M-B" . awesome-pair-jump-left)))
;   :hook (prog-mode . awesome-pair-mode))
;; -AwesomePairPac

;; ConfModePac
(use-package conf-mode
  :ensure nil
  :bind
  (:map conf-mode-map
        (("M-D" . awesome-pair-kill)
         ("SPC" . awesome-pair-space)
         ("=" . awesome-pair-equal)
         ("M-F" . awesome-pair-jump-right)
         ("M-B" . awesome-pair-jump-left))))
;; -ConfModePac

;; DeleteBlockPac
(use-package delete-block
  :straight nil
  :load-path (lambda () (expand-file-name "site-elisp/delete-block" user-emacs-directory))
  :bind
  (("M-d" . delete-block-forward)
   ("C-<backspace>" . delete-block-backward)
   ("M-<backspace>" . delete-block-backward)
   ("M-DEL" . delete-block-backward)))
;; -DeleteBlockPac

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
