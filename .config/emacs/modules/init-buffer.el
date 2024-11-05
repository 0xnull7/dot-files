;; ;;; init-buffer.el --- -*- lexical-binding: t -*-
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;;; Commentary:
;; ;;
;; ;; This file initializes ibuffer and ibuffer-vc
;; ;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;
;; ;;; Code:

;; (eval-when-compile
;;   (require 'use-package))

;; ;; IBufferPac
;; (use-package ibuffer
;;   :ensure nil
;;   :bind ("C-x C-b" . ibuffer)
;;   :init
;;   (use-package ibuffer-vc
;;     :commands (ibuffer-vc-set-filter-groups-by-vc-root)
;;     :custom
;;     (ibuffer-vc-skip-if-remote 'nil))
;;   :custom
;;   (ibuffer-formats
;;    '((mark modified read-only locked " "
;;            (name 35 35 :left :elide)
;;            " "
;;            (size 9 -1 :right)
;;            " "
;;            (mode 16 16 :left :elide)
;;            " " filename-and-process)
;;      (mark " "
;;            (name 16 -1)
;;            " " filename))))
;; ;; -IBufferPac

;; (provide 'init-buffer)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; init-buffer.el ends here
