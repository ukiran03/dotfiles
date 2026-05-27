;;; init-langs.el --- Language specific configurations.	-*- lexical-binding: t -*-

;;; Commentary:

;;;;; Ephimeral Language setup
(use-package sxhkdrc-mode)
(use-package markdown-mode)
(use-package python-mode)
(use-package just-mode)

(use-package conf-xdefaults-mode
  :ensure nil
  :mode "\\`\\(xresources\\|xmodmap\\)\\'")

(provide 'init-langs)
;;; init-langs.el ends here
