;;; init-ein.el --- -*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes Jupyter (formerly IPython) client for Emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'use-package))

;; EINPac
(use-package ein
  :if (executable-find "jupyter")
  :bind
  (("C-c e" . ein:worksheet-execute-cell)
   ("C-c C-e" . ein:worksheet-execute-all-cells))
  :custom-face
  (ein:basecell-input-area-face ((t (:extend t :background "#303640"))))
  :defer t
  :custom
  (ein:worksheet-enable-undo t))
;; -EINPac

(provide 'init-ein)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ein.el ends here
