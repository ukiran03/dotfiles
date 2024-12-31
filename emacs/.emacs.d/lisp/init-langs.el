;;; init-langs.el --- Language specific configurations.	-*- lexical-binding: t -*-

;;; Commentary: Programming language setup

                                        ; -- ephimeral langs
(use-package sxhkdrc-mode)
(use-package markdown-mode)

;; -- Python
(use-package python-mode
  :ensure nil
  :init
  (use-package eglot
    :config
    (add-to-list 'eglot-server-programs
                 '(python-mode . ("ruff" "server"))))
  :hook ((python-mode . eglot-ensure)
         (after-save-hook . eglot-format)
         (python-mode . flymake-ruff-load)))

;; -- Rust

;; -- GO

;; -- Bash

(provide 'init-langs)
;;; init-langs.el ends here
