;;; init-langs.el --- Language specific configurations.	-*- lexical-binding: t -*-

;;; Commentary: Programming language setup

                                        ; -- ephimeral langs
(use-package sxhkdrc-mode)
(use-package markdown-mode)
(use-package python-mode)
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
  ;; (setq haskell-process-name "/usr/bin/ghci")
  ;; (setq haskell-process-name "~/.ghcup/bin/ghci")
  ;; (add-to-list 'load-path "/path/to/hindent/site-lisp")
  (use-package hindent
    :load-path ("site-lisp/hindent")
    :hook (haskell-mode . hindent-mode))
  (use-package hs-lint
    :load-path ("site-lisp/hlint")
    ))

;; -- Forth
(use-package forth-mode
  :init
  (use-package forth-block-mode
    :ensure nil)
  (use-package forth-interaction-mode
    :ensure nil))

(provide 'init-langs)
;;; init-langs.el ends here
