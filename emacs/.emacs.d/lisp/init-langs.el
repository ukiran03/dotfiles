;;; init-langs.el --- Language specific configurations.	-*- lexical-binding: t -*-

;;; Commentary: Programming language setup

(use-package sxhkdrc-mode)

(use-package kdl-ts-mode
  :disabled
  :load-path ("site-lisp/kdl-ts-mode/")
  :config
  (add-to-list 'auto-mode-alist '("\\.kdl\\'" . kdl-ts-mode)))

(use-package kdl-mode
  :ensure nil
  :demand t
  :commands kdl-mode
  :load-path ("site-lisp/kdl-mode/"))

(provide 'init-langs)

;;; init-langs.el ends here
