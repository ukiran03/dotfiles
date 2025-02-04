;;; init-langs.el --- Language specific configurations.	-*- lexical-binding: t -*-

;;; Commentary: Programming language setup

                                        ; -- ephimeral langs
(use-package sxhkdrc-mode)
(use-package markdown-mode)

;; -- Python
;; (use-package python-mode
;;   :ensure t
;;   :init
;;   (use-package eglot
;;     :config
;;     (add-to-list 'eglot-server-programs
;;                  '(python-mode . ("ruff" "server"))))
;;   :hook ((python-mode . eglot-ensure)
;;          (after-save-hook . eglot-format)
;;          (python-mode . flymake-ruff-load)))

;; -- Rust

;; -- GO

;; -- Bash
(use-package haskell-mode
  :config
  (setq haskell-process-name "/usr/bin/ghci")
  (use-package hindent))

(provide 'init-langs)
;;; init-langs.el ends here
