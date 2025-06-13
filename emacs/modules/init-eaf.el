;;; init-eaf.el --- -*- lexical-binding: t -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes Emacs Application Framework
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-global-config)
  (require 'use-package))

;; EAFPac
(use-package eaf
  :load-path (lambda () (expand-file-name "site-elisp/emacs-application-framework" user-emacs-directory))
  :if eaf-env-p
  :custom
  (eaf-start-python-process-when-require nil)
  (browse-url-browser-function #'eaf-open-browser) ;; Make EAF Browser my default browser
  (eaf-start-python-process-when-require t)
  (eaf-browser-dark-mode nil)
  (eaf-browser-enable-adblocker t)
  (eaf-webengine-continue-where-left-off t)
  (eaf-webengine-default-zoom 1.25)
  (eaf-webengine-scroll-step 200)
  (eaf-pdf-dark-mode "ignore")
  :demand
  :bind
  (("M-z r" . eaf-open-rss-reader)
   ("M-m r" . eaf-open-rss-reader)
   ("M-#" . eaf-open-pyqterminal))
  :config
  ;; Require all EAF apps unconditionally, change to apps you're interested in.
  (require 'eaf-file-manager nil t)
  (require 'eaf-music-player nil t)
  (require 'eaf-image-viewer nil t)
  (require 'eaf-camera nil t)
  (require 'eaf-demo nil t)
  (require 'eaf-airshare nil t)
  (require 'eaf-markdown-previewer nil t)
  (require 'eaf-video-player nil t)
  (require 'eaf-vue-demo nil t)
  (require 'eaf-file-sender nil t)
  (require 'eaf-pdf-viewer nil t)
  (require 'eaf-mindmap nil t)
  (require 'eaf-netease-cloud-music nil t)
  (require 'eaf-jupyter nil t)
  (require 'eaf-org-previewer nil t)
  (require 'eaf-system-monitor nil t)
  (require 'eaf-rss-reader nil t)
  (require 'eaf-pyqterminal nil t)
  (require 'eaf-file-browser nil t)
  (require 'eaf-browser nil t)
  (require 'eaf-git nil t)
  (when (display-graphic-p)
    (require 'eaf-all-the-icons))
  (defalias 'browse-web #'eaf-open-browser)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  (eaf-bind-key nil "M-z" eaf-browser-keybinding)
  (eaf-bind-key open_link "C-M-s" eaf-browser-keybinding)
  (eaf-bind-key open_devtools "M-i" eaf-browser-keybinding)
  (eaf-bind-key insert_or_recover_prev_close_page "X" eaf-browser-keybinding)
  (eaf-bind-key scroll_up "RET" eaf-pdf-viewer-keybinding)
  (eaf-bind-key delete_cookies "C-M-q" eaf-browser-keybinding)
  (eaf-bind-key delete_all_cookies "C-M-Q" eaf-browser-keybinding)
  (eaf-bind-key clear_history "C-M-p" eaf-browser-keybinding)
  (eaf-bind-key scroll_down_page "DEL" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down_page "u" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_up_page "d" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_to_end "M->" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_to_begin "M-<" eaf-pdf-viewer-keybinding)
  (eaf-bind-key quit-window "q" eaf-pdf-viewer-keybinding)
  (eaf-bind-key zoom_in "C-=" eaf-pdf-viewer-keybinding)
  (eaf-bind-key zoom_out "C--" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding))
;; -EAFPac

;; PopwebPac
(use-package popweb
  :if eaf-env-p
  :load-path (lambda () (expand-file-name "site-elisp/popweb" user-emacs-directory))
  :init
  (add-to-list 'load-path (expand-file-name "site-elisp/popweb/extension/latex" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "site-elisp/popweb/extension/dict" user-emacs-directory))
  (require 'popweb-latex)
  (require 'popweb-dict-youdao)
  :custom
  (popweb-popup-pos "point-bottom")
  :hook ((org-mode . popweb-latex-mode)
         (tex-mode . popweb-latex-mode)
         (ein:markdown-mode . popweb-latex-mode))
  )
;; -PopwebPac

(provide 'init-eaf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eaf.el ends here
