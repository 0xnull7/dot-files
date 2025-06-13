;;; init-mu4e.el --- Email Client Configuration with Mu4e -*- lexical-binding: t; -*-

;;; Commentary:
;; This file initializes and configures mu4e for email management in Emacs,
;; including alert notifications, thread folding, and context-specific settings
;; for different accounts (e.g., Gmail).

;;; Code:

;; --- 1. Mu4e (Core Email Client) ---
(use-package mu4e
  :straight nil
  :commands (mu4e mu4e-main)
  :bind
  ("C-x m" . mu4e) ; Standard Emacs mail binding.
  ("M-m m" . mu4e) ; Your custom binding.
  (:map mu4e-view-mode-map
        ("e" . mu4e-view-save-attachment)) ; Save attachments easily.

  :custom
  ;; Path to your Maildir directory.
  (mu4e-maildir (expand-file-name "~/Maildir"))
  ;; Command to fetch new mail. Ensure `mbsync` is installed and configured.
  (mu4e-get-mail-command "mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a")
  ;; Prefer HTML rendering for email viewing (requires `shr.el` or `eww`).
  (mu4e-view-prefer-html t)
  ;; How often Mu4e should check for new mail (in seconds).
  (mu4e-update-interval 180) ; 3 minutes.
  (mu4e-headers-auto-update t) ; Automatically update headers in the headers buffer.
  (mu4e-compose-format-flowed t) ; Use format=flowed for easier plain-text replies.
  (mu4e-view-show-images t) ; Display images in HTML emails (requires `ImageMagick` usually).
  ;; Rename files when moving emails; useful for `mbsync`.
  (mu4e-change-filenames-when-moving t)
  ;; Default directory for saving attachments.
  (mu4e-attachment-dir (expand-file-name "~/Downloads"))
  ;; Kill the message buffer after sending.
  (message-kill-buffer-on-exit t)
  ;; Prevent replying to yourself by default.
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-view-show-addresses t) ; Show full email addresses in view mode.
  (mu4e-confirm-quit nil) ; Don't ask for confirmation when quitting Mu4e.
  (mu4e-use-fancy-chars t) ; Use Unicode characters for UI elements.
  (mu4e-headers-results-limit 1000) ; Limit the number of headers displayed.
  (mu4e-view-use-gnus t) ; Use Gnus for rendering email (especially for HTML/MIME).

  ;; Gnus iCalendar integration for Org-mode.
  ;; Ensure these files/headlines exist in your Org-mode setup.
  (gnus-icalendar-org-capture-file (expand-file-name "~/org/agenda/meetings.org"))
  (gnus-icalendar-org-capture-headline '("Meetings"))

  ;; Configure column display in the headers buffer.
  ;; This lambda needs to be wrapped in a hook for it to apply dynamically.
  (mu4e-headers-fields
   `((:human-date . 25) ; Date column width.
     (:flags . 6)        ; Flags column width.
     (:from . 22)        ; From column width.
     ;; Subject column takes remaining width, adjusted for other columns' width.
     (:thread-subject . ,(- (window-body-width) 70))
     (:size . 7)))       ; Size column width.

  :hook
  ;; Hooks run when specific Mu4e modes are active.
  (mu4e-view-mode . visual-line-mode) ; Wrap lines in view mode.
  (mu4e-compose-mode . (lambda ()
                          (visual-line-mode) ; Wrap lines in compose mode.
                          (use-hard-newlines -1) ; Do not use hard newlines.
                          (flyspell-mode))) ; Enable spell-checking in compose mode.
  (mu4e-view-mode . (lambda() ;; Emulate `eww` keybindings for navigation.
                      (local-set-key (kbd "<tab>") 'shr-next-link)
                      (local-set-key (kbd "<backtab>") 'shr-previous-link)))

  :config
  ;; Ensure Gnus iCalendar and Mu4e iCalendar are set up for event handling.
  (require 'mu4e-icalendar)
  (setq mail-user-agent (mu4e-user-agent)) ; Set Emacs's mail agent to Mu4e.
  (mu4e-icalendar-setup)
  (gnus-icalendar-org-setup)

  ;; Provide a more discoverable alias for adding attachments.
  (defalias 'mu4e-add-attachment 'message-add-attachment
    "I prefer the add-attachment function to begin with mu4e so I can find it easily.")

  ;; Add "View in Browser" to message view actions.
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; --- Contexts for Multiple Email Accounts ---
  ;; Define contexts for different email accounts (e.g., Gmail, work, personal).
  ;; This enables switching between accounts seamlessly with `mu4e-context-switch`.
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "gmail" ; A friendly name for this context.
          :enter-func (lambda () (mu4e-message "Entering context gmail"))
          :leave-func (lambda () (mu4e-message "Leaving context gmail"))
          :match-func
          ;; Match messages belonging to this context based on maildir.
          (lambda (msg)
            (when msg
              (string-match "gmail" (mu4e-message-field msg :maildir))))
          :vars '(
                  ;; Maildir paths specific to this account.
                  (mu4e-sent-folder . "/gmail/Sent Mail")
                  (mu4e-drafts-folder . "/gmail/Drafts")
                  (mu4e-trash-folder . "/gmail/Trash")
                  (mu4e-sent-messages-behavior . sent)
                  ;; Signature for this account (can be a string or a variable).
                  (mu4e-compose-signature . user-full-name) ; Or a specific string.
                  ;; Your email address for this context. IMPORTANT: Set this!
                  (user-mail-address . "your.email@gmail.com") ; <<-- CHANGE THIS
                  (mu4e-compose-format-flowed . t)
                  ;; SMTP settings for sending mail for this context.
                  (smtpmail-queue-dir . "~/Maildir/gmail/queue/cur")
                  (message-send-mail-function . smtpmail-send-it)
                  (smtpmail-smtp-user . "your.username") ; <<-- CHANGE THIS
                  (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
                  ;; Path to your GPG-encrypted authinfo file.
                  (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
                  (smtpmail-default-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-debug-info . t) ; Enable debug logging for SMTP.
                  (smtpmail-debug-verbose . t) ; More verbose SMTP debug logging.
                  ;; Shortcuts for common folders within this context.
                  (mu4e-maildir-shortcuts . (("/gmail/INBOX" . ?i)
                                            ("/gmail/Sent Mail" . ?s)
                                            ("/gmail/Trash" . ?t)
                                            ("/gmail/All Mail" . ?a)
                                            ("/gmail/Starred" . ?r)
                                            ("/gmail/Drafts" . ?d))))
         ;; Add more `make-mu4e-context` blocks here for other accounts.
         )))

;; --- 2. `mu4e-alert` (Desktop Notifications) ---
;; Located here for better logical grouping of core `mu4e` components.
(use-package mu4e-alert
  :straight t ; Assuming this is on MELPA or available via straight.el.
  :after mu4e ; Load after `mu4e`.
  :if (executable-find "notify-send") ; Only load if notification tool is available.
  :custom
  (mu4e-alert-set-default-style 'libnotify) ; Use `libnotify` for desktop notifications.
  :hook
  ;; Enable notifications and mode-line display after Emacs initialization.
  ((after-init . mu4e-alert-enable-notifications)
   (after-init . mu4e-alert-enable-mode-line-display)))

;; --- 3. `mu4e-overview` (Mu4e Overview Buffer) ---
;; Useful for showing a summary of unread messages across contexts.
(use-package mu4e-overview
  :straight t ; Assuming this is on MELPA or available via straight.el.
  :after mu4e ; Load after `mu4e`.
  :commands (mu4e-overview) ; Lazy load.
  ;; You might want to bind `mu4e-overview` to a key:
  ;; :bind ("C-c o" . mu4e-overview)
  )

;; --- 4. `mu4e-thread-folding` (Collapsible Threads) ---
;; Placed after core `mu4e` for better organization.
(use-package mu4e-thread-folding
  ;; `:straight nil` and `:load-path` are appropriate if this is a local file.
  :straight nil
  :load-path (lambda () (expand-file-name "site-elisp/mu4e-thread-folding" user-emacs-directory))
  :after mu4e ; Ensure `mu4e` is loaded first.
  :bind
  ;; Keybindings for folding/unfolding threads in headers and search modes.
  ((:map mu4e-headers-mode-map
         ("TAB" . mu4e-headers-toggle-at-point)
         ("C-<tab>" . mu4e-headers-toggle-fold-all))
   (:map mu4e-search-minor-mode-map
         ("S" . mu4e-kill-update-mail))) ; Unclear purpose of `S` here, might be a typo.

  :custom
  (mu4e-thread-folding-default-view `folded) ; Threads are folded by default.
  ;; Headers fields for `mu4e-thread-folding` if different from main `mu4e-headers-fields`.
  (mu4e-headers-fields '((:empty . 2)
                         (:human-date . 12)
                         (:flags . 6)
                         (:mailing-list . 10)
                         (:from . 22)
                         (:subject . nil))) ; `nil` means it takes remaining width.
  :config
  ;; Add a custom 'Empty' header field for spacing in thread folding display.
  (add-to-list 'mu4e-header-info-custom
               '(:empty . (:name "Empty"
                                 :shortname ""
                                 :function (lambda (msg) " ")))))

(provide 'init-mu4e)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-mu4e.el ends here
