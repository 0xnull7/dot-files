;;; init-completion.el --- Completion Frameworks Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file configures various completion frameworks for Emacs,
;; aiming for a cohesive and efficient completion experience.

;; Code:

;; --- 1. Anzu (Search and Replace Enhancements) ---
;; Provides visual feedback for search and replace operations.
(use-package
 anzu
 :straight t
 :config (global-anzu-mode +1)

 ;; Customize modeline lighter and behavior
 (setq
  anzu-mode-lighter ""
  anzu-deactivate-region t
  anzu-search-threshold 1000
  anzu-replace-threshold 50
  anzu-replace-to-string-separator " => ")

 ;; Remap default isearch functions to Anzu's versions.
 :bind
 (:map
  isearch-mode-map
  ([remap isearch-query-replace] . anzu-isearch-query-replace)
  ([remap isearch-query-replace-regexp]
   .
   anzu-isearch-query-replace-regexp)))

;; --- 2. Company Mode (Code Completion Framework) ---
;; Primary framework for in-buffer code completion.
(use-package
 company
 :straight t
 :diminish company-mode
 ;; Removed :after-init, config moved to :config block
 :config
 ;; General settings for Company Mode behavior and appearance.
 (setq
  company-minimum-prefix-length 1
  company-idle-delay 0.1
  company-show-numbers t
  company-tooltip-align-annotations t
  company-backends '((company-capf company-dabbrev-code)))
 (setq company-show-quick-access t)
 ;; Exclude Company Mode from specific major modes where it's not useful.
 (setq company-global-modes
       '(not shell-mode eaf-mode term-mode vterm-mode))
 (global-company-mode 1) ; Enable Company Mode globally.
 )

;; --- 3. Cape (Completion At Point Extensions) ---
(use-package
 cape
 :straight t
 :after company ; Ensure Cape loads after Company for better integration
 :init
 ;; Binds TAB to a powerful command that tries snippets first, then completion.
 ;; `cape-super-capf` is a versatile function that chains multiple `capf`s.
 ;; Hooking it to `prog-mode-hook` makes it specific to programming modes.
 (add-hook
  'prog-mode-hook
  (lambda ()
    ;; Chain multiple completion functions. `yas-expand` for snippets,
    ;; `company-capf` for company completions, and `cape-dabbrev` for dabbrev.
    (setq-local completion-at-point-functions
                (list
                 (cape-super-capf
                  #'yas-expand #'company-capf #'cape-dabbrev))))))

;; --- 4. Company Box (UI for Company Mode) ---
(use-package
 company-box
 :straight t
 :if (display-graphic-p) ; Only load if Emacs is running in a graphical environment
 :diminish company-box-mode
 :hook (company-mode . company-box-mode) ; Activate Company Box when Company Mode starts
 :config
 (setq
  company-box-doc-delay 0.2
  company-box-backends-colors nil
  company-box-frame-parameters '((internal-border-width . 1)))

 ;; Integrate with `all-the-icons` for fancy completion icons.
 (with-eval-after-load 'all-the-icons
   (setq company-box-transformer
         'company-box-all-the-icons-transformer)) ; Correct transformer function
 )

;; --- 5. Embark (Action Framework for Completions) ---
;; Allows performing actions on minibuffer candidates or buffer objects.
(use-package
 embark
 :straight t
 :bind
 (("C-." . embark-act) ; Universal action key
  ("C-;" . embark-dwim)) ; Contextual action key (Do What I Mean)
 :init
 ;; Add Embark to default completion dispatchers
 (add-hook 'completion-list-mode-hook 'embark-mode) ; Enable embark in completion buffers
 :config
 ;; Customize Embark if needed, e.g., show a transient command menu.
 ;; (setq embark-indicators '(embark-minimal-indicator))
 )

;; --- 6. Embark Consult (Integrates Embark with Consult) ---
(use-package
 embark-consult
 :straight t
 :after (embark consult)) ; Ensure it loads after Embark and Consult


;; --- 7. Marginalia (Annotations for Minibuffer Completions) ---
;; Adds extra information to minibuffer completion candidates (e.g., file permissions).
(use-package
 marginalia
 :straight t
 :after vertico ; Ensure it loads after Vertico
 :init
 (marginalia-mode) ; Activate Marginalia globally
 )

;; --- 8. Which Key (Keybinding Helper) ---
;; Displays available keybindings as you type prefix keys.
(use-package
 which-key
 :straight t
 :custom (which-key-separator " ") (which-key-prefix-prefix "+")
 :config
 (which-key-mode t)) ; Activate Which Key globally.

(provide 'init-completion)
;;; init-completion.el ends here
