;;; custom-set-variables.el --- 
;;
;; Filename: custom-set-variables.el
;; Description:
;; Author: Ismail Mohamed Ajeeb
;; Maintainer:
;; Copyright (C) 2024 Ismail Mohamed Ajeeb
;; Created: Wed Nov 27 04:02:12 2024 (+0200)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 3
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; custom-set-variables.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(+global-word-wrap-mode 1)
 '(LaTeX-document-regexp "nil")
 '(LaTeX-indent-environment-list '(("document" current-indentation)))
 '(LaTeX-indent-level 4)
 '(LaTeX-item-indent 0)
 '(TeX-brace-indent-level 4)
 '(TeX-newline-function 'newline-and-indent)
 '(column-number-mode t)
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(custom-enabled-themes '(monokai))
 '(custom-safe-themes
   '("2b20b4633721cc23869499012a69894293d49e147feeb833663fdc968f240873" "30d174000ea9cbddecd6cc695943afb7dba66b302a14f9db5dd65074e70cc744" "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "c7fd1708e08544d1df2cba59b85bd25263180b19b287489d4f17b9118487e718" "4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882" default))
 '(default-justification 'full)
 '(display-battery-mode t)
 '(display-fill-column-indicator t)
 '(display-fill-column-indicator-column t)
 '(display-time-mode t)
 '(eldoc-documentation-functions nil t nil "Customized with use-package lsp-mode")
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
 '(default ((t (:slant normal :weight bold :height 160 :width normal :family "JetBrains mono"))))
 '(hi-yellow ((t (:background "dim gray" :foreground "gold"))))
 '(highlight-thing ((t (:inherit 'hi-yellow)))))
