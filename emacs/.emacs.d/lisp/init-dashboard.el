;;; init-dashboard.el --- Dashboard Configuration -*- lexical-binding: t -*-

(use-package panel
  :straight (:host github :repo "LuciusChen/panel")
  :custom
  (panel-image-width 250)
  (panel-image-height 250)
  (panel-title "\n")
  (panel-intro-display 'always)
  (panel-intro-lines
   '("┌─┐ ┌┬┐ ┌─┐ ┌─┐ ┌─┐"
     "├┤  │││ ├─┤ │   └─┐"
     "└─┘ ┴ ┴ ┴ ┴ └─┘ └─┘"))
  (panel-intro-help-lines
   '(("H-f f" . "find a file")
     ("C-h i" . "View info manuals")
     ("q"     . "close this panel")))

  ;; Uncomment to use a custom avatar image:
  ;; (panel-image-file
  ;;  (expand-file-name "avatars/musashi1.jpg" user-emacs-directory))

  :hook
  (panel-mode . (lambda () (setq-local global-hl-line-mode nil)))

  :config
  (panel-create-hook))

(use-package emacs
  :straight (:type built-in)
  :custom
  (initial-buffer-choice
   (lambda ()
     (when (fboundp 'panel-create-hook)
       (panel-create-hook))
     (get-buffer-create "*panel*"))))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
