;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-


;;; Commentary:
;;
;; Customization.
;;

;;; Code:

;; (eval-when-compile
;; (require 'package))

(defcustom ukiran-icon t
  "Display icons or not."
  :group 'centaur
  :type 'boolean)

(defcustom ukiran-completion-style 'minibuffer
  "Completion display style."
  :group 'centaur
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Child Frame" childframe)))

(defcustom ukiran-frame-maximized-on-startup nil
  "Maximize frame on startup or not."
  :group 'centaur
  :type 'boolean)


;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (load custom-file)

(provide 'init-custom)
