;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-


;;; Commentary:
;;
;; Customization.
;;

;;; Code:

;; (eval-when-compile
  ;; (require 'package))


;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(provide 'init-custom)
