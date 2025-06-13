;;; init-erc.el --- Configuration for ERC (Emacs IRC Client) -*- lexical-binding: t; -*-

;;; Commentary:
;; This file initializes and configures ERC, the Emacs IRC client,
;; including settings for appearance, logging, and notifications.

;;; Code:

(use-package erc
  :straight nil
  :ensure nil

  :init
  (defcustom my-irc-nick "0xRoot_7" ; Placeholder: SET YOUR ACTUAL NICKNAME HERE
    "The nickname used to login into ERC."
    :type 'string
    :group 'erc)

  ;; Load ERC helper packages with deferral as they are minor modes/features.
  (use-package erc-hl-nicks :straight t :defer t)
  (use-package erc-image :straight t :defer t)
  (use-package notifications :straight t :commands alert) ; Ensure notifications package is available for `alert`

  ;; Set default values for ERC variables using `:custom`.
  :custom
  (erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs"))) ; Channels to auto-join.
  (erc-user-full-name user-full-name)                             ; Uses `user-full-name` from init-const.el.
  (erc-track-exclude-types '("NICK" "PART" "MODE" "324" "329" "332" "333" "353" "477"))
  (erc-server-coding-system '(utf-8 . utf-8))
  (erc-interpret-mirc-color t)
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-autojoin-timing 'ident)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 15)
  (erc-lurker-threshold-time 43200)
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-prompt-for-password nil)
  (erc-prompt-for-nickserv-password nil) ; Only set to t if you have NickServ setup
  (erc-fill-column 100)
  (erc-save-buffer-on-part t)
  (erc-nick-uniquifier "_")
  ;; Use `locate-user-emacs-file` for consistent path handling
  (erc-log-channels-directory (locate-user-emacs-file ".erc-logs/"))

  ;; Customize faces for visual distinction.
  :custom-face
  (erc-notice-face ((t (:foreground "#ababab"))))

  :bind
  ;; Global keybindings for ERC startup and buffer switching.
  (("M-z i" . erc-start-or-switch)
   ("M-m i" . erc-start-or-switch)
   ("C-c C-b" . erc-switch-to-buffer)
   ;; Keybinding specific to `erc-mode-map`
   (:map erc-mode-map
         ("M-RET" . newline))) ; M-RET is often used for multi-line input in IRC

  :hook
  ;; Use the `notifications` package's hook for ERC notifications.
  (ercn-notify . erc-notify)

  :config
  ;; Ensure the log directory exists before ERC tries to write to it.
  (unless (file-directory-p erc-log-channels-directory)
    (make-directory erc-log-channels-directory t))

  ;; Enable ERC's built-in minor modes for tracking and services.
  (erc-track-mode t)
  (erc-services-mode 1)
  ;; `notifications` module is added automatically by `(use-package notifications)` if `:commands alert` was used.
  ;; `(add-to-list 'erc-modules 'notifications)` might be redundant if `notifications` package is loaded.

  ;; Define custom function to start or switch to ERC.
  (defun erc-start-or-switch ()
    "Start ERC or switch to an existing ERC buffer."
    (interactive)
    (let ((target-buffer (get-buffer "irc.libera.chat:6697")))
      (if target-buffer
          (erc-track-switch-buffer 1) ; Switch to first unread buffer if tracking is active
        ;; Start new ERC session with TLS.
        (erc-tls :server "irc.libera.chat"
                 :port 6697
                 :nick my-irc-nick
                 :full-name user-full-name))))

  ;; Define custom notification function.
  (defun erc-notify (nickname message)
    "Displays a desktop notification message for ERC."
    (when (fboundp 'alert) ; Check if `alert` (from notifications package) is available
      (let* ((channel (buffer-name))
             (nick (erc-hl-nicks-trim-irc-nick nickname))
             (title (if (string-match-p (concat "^" nickname) channel)
                        nick ; If it's a private message, title is just nick
                      (concat nick " (" channel ")"))) ; Otherwise, include channel
             (msg (s-trim (s-collapse-whitespace message)))) ; Clean up message
        (alert (concat nick ": " msg) :title title))))
  ) ; End of `use-package erc` block

(provide 'init-erc)
;;; init-erc.el ends here
