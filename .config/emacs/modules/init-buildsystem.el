;;; init-buildsystem.el --- -*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes docker dockerfile-mode jenkinsfile-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'use-package))
;; DockerPac
(use-package docker :defer t)
;; -DockerPac

;; DockerfilePac
(use-package dockerfile-mode :defer t)
;; -DockerfilePac

;; GroovyPac
(use-package groovy-mode :defer t)
;; -GroovyPac

;; CMakePac
(use-package cmake-mode :defer t)
;; -CMakePac

;; BazelPac
(use-package bazel :defer t)
;; -BazelPac

;; YamlPac
(use-package yaml-mode
  :defer t
  :commands (yaml-get-path-at-point)
  :mode "\\.yml\\'"
  :config
  (use-package yaml-pro
    :hook (yaml-mode . yaml-pro-mode)
    :bind (("C-c M-p" . yaml-pro-move-subtree-up)
           ("C-c M-n" . yaml-pro-move-subtree-down)))
  ;; Based on https://github.com/chopmo/dotfiles/blob/master/.emacs.d/customizations/yaml.el
  (defun yaml-indentation-level (s)
    (if (string-match "^ " s)
        (+ 1 (yaml-indentation-level (substring s 1)))
      0))
  (defun yaml-clean-string (s)
    (let* ((s (replace-regexp-in-string "^[ -:]*" "" s))
           (s (replace-regexp-in-string ":$" "" s)))
      s))
  (defun yaml-path-at-point ()
    (save-excursion
      (let* ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
             (level (yaml-indentation-level line))
             result)
        (while (> (point) (point-min))
          (beginning-of-line 0)
          (setq line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
          (let ((new-level (yaml-indentation-level line)))
            (when (and (string-match "[^[:blank:]]" line)
                       (< new-level level))
              (setq level new-level)
              (setq result (push (yaml-clean-string line) result)))))
        (mapconcat 'identity result " => "))))
  (defun yaml-get-path-at-point ()
    "Display the yaml path at point for 5 seconds"
    (interactive)
    (let ((ov (display-line-overlay+ (window-start) (yaml-path-at-point))))
      (run-with-timer 1 nil (lambda () (when (overlayp ov)
                                         (delete-overlay ov)))))))
;; -YamlPac

(provide 'init-buildsystem)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-buildsystem.el ends here
