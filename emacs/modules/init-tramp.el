;;; init-tramp.el --- TRAMP Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file initializes and customizes TRAMP, Emacs's Transparent Remote Access
;; feature, including a custom method for Google Cloud SSH.

;;; Code:

;; --- 1. `tramp` (Transparent Remote Access, Multiple Protocols) ---
(use-package tramp
  :straight nil
  :defer 1

  :config
  ;; Ensure TRAMP inherits the PATH from the remote machine's environment.
  ;; This is crucial for remote commands to find executables.
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; Add a custom TRAMP method for Google Cloud SSH (`gcloud compute ssh`).
  ;; This allows you to open files like `/gssh:instance-name:/path/to/file`.
  (add-to-list 'tramp-methods
               '("gssh"                            ; The TRAMP method name (e.g., /gssh:user@host:/path)
                 (tramp-login-program "gcloud compute ssh") ; The actual command to log in.
                 (tramp-login-args (("%h")))        ; Arguments for the login program (here, just the host).
                 (tramp-async-args (("-q")))        ; Arguments for asynchronous connection (quiet mode).
                 (tramp-remote-shell "/bin/bash")   ; Default shell on the remote machine.
                 (tramp-remote-shell-args ("-c"))   ; Arguments for the remote shell.
                 ;; Gateway arguments for `gcloud compute ssh`. These might be specific to your setup.
                 (tramp-gw-args
                  (("-o" "GlobalKnownHostsFile=/dev/null")
                   ("-o" "UserKnownHostsFile=/dev/null")
                   ("-o" "StrictHostKeyChecking=no"))) ; Disable host key checking for convenience (use with caution).
                 (tramp-default-port 22)))          ; Default port for the connection.

  ;; --- Optional TRAMP Customizations ---
  ;; Increase the TRAMP connection timeout (default is 10 seconds).
  ;; Useful for slower connections or busy servers.
  ;; (setq tramp-connection-timeout 30)

  ;; Make TRAMP use a persistent connection by default, which can speed up operations.
  ;; (setq tramp-default-method "sshx")

  ;; Disable password caching if you prefer to enter passwords every time.
  ;; (setq tramp-save-password nil)

  ;; Configure specific shells for certain remote hosts or methods.
  ;; (add-to-list 'tramp-shell-file-name-regexp-alist
  ;;              (list "ssh" "/bin/bash")) ; Example: force bash for generic ssh.
  )

(provide 'init-tramp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tramp.el ends here
