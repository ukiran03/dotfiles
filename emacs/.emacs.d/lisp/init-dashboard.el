(use-package panel
  :straight (:host github :repo "LuciusChen/panel")
  :config
  (panel-create-hook)
  (add-hook 'panel-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (setq panel-image-width 250
        panel-title "\n"
        panel-intro-display 'always
        panel-intro-lines
        '("┌─┐ ┌┬┐ ┌─┐ ┌─┐ ┌─┐"
          "├┤  │││ ├─┤ │   └─┐"
          "└─┘ ┴ ┴ ┴ ┴ └─┘ └─┘")
        panel-intro-help-lines
        '(("H-f f" . "find a file")
          ("C-h i" . "View info manuals")
          ("q"     . "close this panel"))
        panel-image-height 250))

;; (setq panel-image-file
;;       (concat
;;        (expand-file-name "avatars/" user-emacs-directory)
;;        "musashi1.jpg"))

(setq initial-buffer-choice
      (lambda ()
        (when (fboundp 'panel-create-hook)
          (panel-create-hook))
        (get-buffer-create "*panel*")))

(provide 'init-dashboard)
