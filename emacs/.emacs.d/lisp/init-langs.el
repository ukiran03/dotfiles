;;; init-langs.el --- Language specific configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; Configurations for auxiliary programming and configuration languages.

;;; Code:

;; Ephemeral Language Setup
(use-package sxhkdrc-mode
  :straight t)

(use-package markdown-mode
  :straight t)

(use-package just-mode
  :straight t)

(use-package dockerfile-mode
  :straight t)

;; TODO: https://github.com/zkry/yaml-pro

(use-package yaml-mode
  :straight t
  :hook ((yaml-mode . indent-bars-mode)
         (yaml-mode . symbol-overlay-mode)))
;; (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))

(use-package conf-xdefaults-mode
  :straight (:type built-in)
  :mode "\\`\\(xresources\\|xmodmap\\)\\'")

(provide 'init-langs)
;;; init-langs.el ends here
