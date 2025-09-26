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
  (setq go-ts-mode-indent-offset 4)
  (use-package go-mode
    :ensure t)
  :bind (:map go-ts-mode-map
              ("C-c C-a" . go-import-add)
              ("C-c C-d" . godef-describe)
              ("C-c C-j" . godef-jump)
              ("C-M-q"   . prog-indent-sexp)
              ("M-q"     . prog-fill-reindent-defun)
              ("C-c C-f a" . go-goto-arguments)
              ("C-c C-f d" . go-goto-docstring)
              ("C-c C-f f" . go-goto-function)
              ("C-c C-f i" . go-goto-imports)
              ("C-c C-f m" . go-goto-method-receiver)
              ("C-c C-f n" . go-goto-function-name)
              ("C-c C-f r" . go-goto-return-values)
              ("C-x 4 C-c C-j" . godef-jump-other-window)
              ))

(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

(use-package gotest
  :ensure t)

(provide 'init-go)
