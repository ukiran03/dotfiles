;; init-go.el --- Initialize Golang configurations.	-*- lexical-binding: t -*-

(use-package go-ts-mode
  :ensure nil
  :mode ("\\.go\\'" . go-ts-mode)
  :bind (:map go-ts-mode-map
              ("<f1>" . compile))
  :interpreter ("go" . go-ts-mode)
  :hook ((go-ts-mode . apheleia-mode)
         (go-ts-mode . symbol-overlay-mode)
         (go-ts-mode . my/disable-super-save)
         (go-ts-mode . (lambda ()
                         (let* ((proj (project-current))
                                (root (when proj (project-root proj))))
                           (when (and root
                                      (file-exists-p (expand-file-name "go.mod" root))
                                      (not (bound-and-true-p eglot--managed-mode)))
                             (eglot-ensure))))))
  :config
  (setq go-ts-mode-indent-offset 4)
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))

  ;; Hook it up to both standard and Tree-sitter Go modes
  (use-package go-mode
    :ensure t)
  :bind (:map go-ts-mode-map
              ("C-c C-a" . go-import-add)
              ("C-c C-d" . godef-describe)
              ("C-c C-j" . godef-jump)
              ("C-c C-r" . go-run)
              ("C-M-q"   . prog-indent-sexp)
              ("M-q"     . prog-fill-reindent-defun)
              ("C-c C-f a" . go-goto-arguments)
              ("C-c C-f d" . go-goto-docstring)
              ("C-c C-f f" . go-goto-function)
              ("C-c C-f i" . go-goto-imports)
              ("C-c C-f m" . go-goto-method-receiver)
              ("C-c C-f n" . go-goto-function-name)
              ("C-c C-f r" . go-goto-return-values)
              ("C-x 4 C-c C-j" . godef-jump-other-window)))

(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

(use-package apheleia
  :config
  ;; go install mvdan.cc/gofumpt@latest
  (setf (alist-get 'uk-go-fmt apheleia-formatters)
        '("golines" "--base-formatter" "gofumpt -extra"))

  (setf (alist-get 'go-mode apheleia-mode-alist)
        '(goimports uk-go-fmt))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist)
        '(goimports uk-go-fmt)))

(use-package gotest
  :ensure t)
;; (use-package go-test
;;   :bind (:map go-mode-map
;;               ("C-c t r" . go-test-current-test)
;;               ("C-c t f" . go-test-current-file))

;; go install github.com/josharian/impl@latest
(use-package go-impl
  :ensure t)

;; go install github.com/fatih/gomodifytags@latest
(use-package go-tag
  :ensure t)

;; go get -u github.com/davidrjenni/reftools/cmd/fillstruct
(use-package go-fill-struct)

(provide 'init-go)
