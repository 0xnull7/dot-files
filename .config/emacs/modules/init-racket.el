;;; init-racket.el --- -*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package racket-mode
  :ensure t)

;;  (add-hook 'racket-mode-hook		
;;            (lambda ()			
;;              (define-key racket-mode-map (kbd "<f5>") 'racket-run)))

(provide 'init-racket)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-racket.el ends here