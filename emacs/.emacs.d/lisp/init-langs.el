;;; init-langs.el --- Language specific configurations.	-*- lexical-binding: t -*-

;;; Commentary:


;; -- Python setup with tree-sitter
(use-package python-base-mode
  :ensure nil
  :hook ((python-base-mode . apheleia-mode)
         (python-base-mode . symbol-overlay-mode))
  :custom
  (python-indent-offset 4))

(use-package python-ts-mode
  :ensure nil
  :hook ((python-base-mode . eglot-ensure))
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode))

;; Remap all python buffers to tree-sitter mode
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))


;; -- Elixir




;;;;; Ephimeral Language setup
;; -- Bash TODO:
(use-package sxhkdrc-mode)
(use-package markdown-mode)
(use-package python-mode)
(use-package conf-xdefaults-mode
  :ensure nil
  :mode "\\`\\(xresources\\|xmodmap\\)\\'")

;;;;; Recreational
;; -- Elm
(use-package elm-mode)
(use-package elm-yasnippets)
(use-package elm-test-runner)
;; -- Forth
(use-package forth-mode
  :init
  (use-package forth-block-mode
    :ensure nil)
  (use-package forth-interaction-mode
    :ensure nil))

;;;;; Not Yet
;; (use-package odin-mode
;;   :ensure nil
;;   :vc (:url "https://git.sr.ht/~mgmarlow/odin-mode")
;;   :bind (:map odin-mode-map
;; 	          ("C-c C-r" . 'odin-run-project)
;; 	          ("C-c C-c" . 'odin-build-project)
;; 	          ("C-c C-t" . 'odin-test-project)))
;; ;; Enable odin-mode and configure OLS as the language server
;; (use-package odin-mode
;;   :ensure (:host github :repo "mattt-b/odin-mode")
;;   :mode ("\\.odin\\'" . odin-mode))

;; -- Haskell
;; (use-package haskell-mode
;;   :config
;;;;   (setq haskell-process-name "/usr/bin/ghci")
;;;;   (setq haskell-process-name "~/.ghcup/bin/ghci")
;;;;   (add-to-list 'load-path "/path/to/hindent/site-lisp")
;;   (use-package hindent
;;     :load-path ("site-lisp/hindent")
;;     :hook (haskell-mode . hindent-mode))
;;   (use-package hs-lint
;;     :load-path ("site-lisp/hlint")))

(provide 'init-langs)
;;; init-langs.el ends here
