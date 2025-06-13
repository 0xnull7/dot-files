;;; init-debbugs.el --- Configuration for debbugs integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides commands to interact with the GNU Bug Tracker (debbugs).

;;; Code:

(use-package debbugs
  :straight t
  :commands (debbugs-gnu debbugs-gnu-browse debbugs-submit))

(provide 'init-debbugs)
;;; init-debbugs.el ends here
