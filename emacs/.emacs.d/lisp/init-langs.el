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
         (yaml-mode . symbol-overlay-mode)
         (yaml-mode . subword-mode)))
;; (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))

(use-package conf-xdefaults-mode
  :straight (:type built-in)
  :mode "\\`\\(xresources\\|xmodmap\\)\\'")

(use-package haskell-mode
  :straight t)

(use-package typst-ts-mode
  :straight t
  :hook ((typst-ts-mode . apheleia-mode)
         (typst-ts-mode . eglot-ensure))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((typst-ts-mode) . ("tinymist" "lsp")))
    (setq eglot-workspace-configuration
          '((tinymist . (:exportPdf "onSave"
                                    :formatterMode "typstyle")))))
  :init
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'typst-ts-mode)
                (setq-local eldoc-documentation-strategy
                            'eldoc-documentation-default)))))

(use-package mermaid-mode
  :straight t)

(use-package elixir-ts-mode
  :straight (:type built-in))

(use-package inf-elixir
  :straight t
  ;; :bind (("C-c i i" . 'inf-elixir)
  ;;        ("C-c i p" . 'inf-elixir-project)
  ;;        ("C-c i l" . 'inf-elixir-send-line)
  ;;        ("C-c i r" . 'inf-elixir-send-region)
  ;;        ("C-c i b" . 'inf-elixir-send-buffer)
  ;;        ("C-c i R" . 'inf-elixir-reload-module))
  )

(provide 'init-langs)
;;; init-langs.el ends here
