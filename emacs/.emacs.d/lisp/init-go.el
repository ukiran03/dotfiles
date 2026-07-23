;;; init-go.el --- summary -*- lexical-binding: t -*-

(use-package compile
  :ensure nil
  :custom
  (compile-command "just "))

;; Core Go-mode navigation fallbacks
(use-package go-mode
  :ensure t
  :defer t
  :commands (godef-jump
             godef-jump-other-window
             godef-describe
             go-import-add
             go-goto-arguments
             go-goto-docstring
             go-goto-function
             go-goto-imports
             go-goto-method-receiver
             go-goto-function-name
             go-goto-return-values))

(use-package gotest
  :ensure t
  ;; :defer t
  :commands (go-run))

(with-eval-after-load 'project
  (add-to-list 'project-vc-ignores "vendor/"))

(use-package go-ts-mode
  :ensure nil
  :mode "\\.go\\'"
  :interpreter "go"
  :hook ((go-ts-mode . symbol-overlay-mode)
         (go-ts-mode . my/go-init-logic)
         (go-ts-mode . puni-mode)
         (go-ts-mode . apheleia-mode))
  :bind (:map go-ts-mode-map
              ("<f1>" . compile)
              ("<f5>" . apheleia-format-buffer)
              ("C-M-q" . prog-indent-sexp)
              ("M-q" . prog-fill-reindent-defun)
              ;; Navigation & Refactoring (Fallback primitives from go-mode)
              ("H-c a" . go-import-add)
              ("H-c d" . godef-describe)
              ("H-c j" . godef-jump)
              ("H-c r" . go-run)
              ("C-x 4 C-c C-j" . godef-jump-other-window)
              ;; "Go-to" Helpers
              ("<f2> a" . go-goto-arguments)
              ("<f2> d" . go-goto-docstring)
              ("<f2> f" . go-goto-function)
              ("<f2> i" . go-goto-imports)
              ("<f2> m" . go-goto-method-receiver)
              ("<f2> n" . go-goto-function-name)
              ("<f2> r" . go-goto-return-values))
  :config
  (setq go-ts-mode-indent-offset 4)

  (defun my/go-init-logic ()
    "Eglot ensure based on go.mod presence."
    (let* ((proj (project-current))
           (proj-indeed (when proj (project-root proj)))
           (sure-eglot (and proj-indeed
                            (file-exists-p
                             (expand-file-name "go.mod" proj-indeed)))))
      (when sure-eglot
        (eglot-ensure))))

  ;; Sync environment variables securely
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY"))))

;; Remap default go-mode to modern tree-sitter equivalent
(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

(use-package apheleia
  :ensure t
  :config
  (setf (alist-get 'go-format apheleia-formatters)
        '("golines" "-m" "79" "--base-formatter" "gofumpt -extra")) ;TODO: add fill-column as a var
  (setf (alist-get 'gotmplfmt apheleia-formatters) '("gotmplfmt"))
  (setf (alist-get 'templ-fmt apheleia-formatters) '("templ" "fmt"))
  (setf (alist-get 'buf-fmt apheleia-formatters) '("buf" "format" "--path" filepath))

  (setf (alist-get 'go-mode apheleia-mode-alist) '(goimports go-format))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) '(goimports go-format))
  (setf (alist-get 'web-mode apheleia-mode-alist) '(gotmplfmt))
  (setf (alist-get 'protobuf-mode apheleia-mode-alist) '(buf-fmt))
  (setf (alist-get 'templ-ts-mode apheleia-mode-alist) '(templ-fmt)))

(use-package templ-ts-mode
  :mode ("\\.templ\\'" . templ-ts-mode)
  :hook ((templ-ts-mode . eglot-ensure)
         (templ-ts-mode . apheleia-mode)
         (templ-ts-mode . emmet-mode))
  :config
  (with-eval-after-load 'eglot
    (defvar eglot-server-programs)
    (add-to-list 'eglot-server-programs
                 '(templ-ts-mode . ("templ" "lsp")))))

;; Structural Go Helper packages
(use-package go-impl :ensure t :defer t)
(use-package go-tag :ensure t :defer t)
(use-package go-fill-struct :ensure t :defer t)

;; Protocol Buffers & gRPC configs
(use-package protobuf-mode
  :ensure t
  :blackout (protobuf-mode . "PBuf")
  :mode ("\\.proto\\'" . protobuf-mode)
  :hook ((protobuf-mode . eglot-ensure)
         (protobuf-mode . apheleia-mode))
  :config
  (with-eval-after-load 'eglot
    (defvar eglot-server-programs)
    (add-to-list 'eglot-server-programs
                 '(protobuf-mode . ("buf" "lsp" "serve")))))

;; Tempel Snippets
(use-package tempel
  :ensure t
  :init
  ;; Define a function that returns your templates (Tempel loves this)
  (defun my-golang-templates ()
    '((go "go func() {" n> q n> "}()")
      (defer "defer func() {" n> q n> "}()")))

  :config
  ;; Safely add it as a buffer-local source when go-ts-mode activates
  (add-hook 'go-ts-mode-hook
            (lambda ()
              (add-hook 'tempel-template-sources
                        #'my-golang-templates nil 'local))))

(provide 'init-go)

;;;;;; Go Tools Setup Reminder
;; go install mvdan.cc/gofumpt@latest
;; go install github.com/segmentio/golines@latest
;; go install github.com/josharian/impl@latest
;; go install github.com/fatih/gomodifytags@latest
;; go install github.com/davidrjenni/reftools/cmd/fillstruct@latest
