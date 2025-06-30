;;; init-leetcode.el --- -*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes a LeetCode client
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'use-package))

;; LeetCodePac
(use-package leetcode
  :straight nil
  :load-path (lambda () (expand-file-name "site-elisp/leetcode.el" user-emacs-directory))
  :commands (leetcode)
  :init
  (use-package graphql :defer t)
  (use-package aio :defer t)
  :custom
  (url-debug t)
  (leetcode-prefer-language "python3"))
;; -LeetCodePac

(provide 'init-leetcode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-leetcode.el ends here
