;;; init-langs.el --- Language specific configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;;;; Ephimeral Language setup
(use-package sxhkdrc-mode)
(use-package markdown-mode)
(use-package just-mode)

(use-package dockerfile-mode)

;; TODO: https://github.com/zkry/yaml-pro

(use-package yaml-mode
  :hook ((yaml-mode . indent-bars-mode)
         (yaml-mode . symbol-overlay-mode)))
;; (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))

(use-package conf-xdefaults-mode
  :straight nil
  :mode "\\`\\(xresources\\|xmodmap\\)\\'")

(provide 'init-langs)
;;; init-langs.el ends here
