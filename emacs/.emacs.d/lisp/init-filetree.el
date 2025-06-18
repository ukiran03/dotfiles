;; init-filetree.el --- Initialize filetree.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Filetree: A tree layout file explorer.
;;

;;; Code:

(eval-when-compile
  (require 'init-funcs))

(use-package projtree
  :disabled
  :load-path ("site-lisp/emacs-projtree/")
  :ensure nil
  :commands (projtree-mode))

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :custom
  (dired-sidebar-window-fixed nil)
  (dired-sidebar-width 25)
  (dired-sidebar-theme 'none)
  :bind
  ([f8] . dired-sidebar-toggle-with-current-directory))



(provide 'init-filetree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-filetree.el ends here
