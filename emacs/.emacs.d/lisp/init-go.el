;; init-go.el --- Initialize Golang configurations.	-*- lexical-binding: t -*-

(use-package go-ts-mode
  :ensure nil
  :mode ("\\.go\\'" . go-ts-mode)
  :bind (:map go-ts-mode-map
              ("<f1>" . godoc))
  :interpreter ("go" . go-ts-mode)
  :hook ((go-ts-mode . apheleia-mode)
         (go-ts-mode . symbol-overlay-mode)
         (go-ts-mode . my/disable-super-save))
  :config
  (setq go-ts-mode-indent-offset 4))

(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))


(provide 'init-go)
