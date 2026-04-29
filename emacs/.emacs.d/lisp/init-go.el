;;; init-go.el --- summary -*- lexical-binding: t -*-

(use-package compile
  :ensure nil
  :custom
  (compile-command "just "))

(use-package go-mode
  :ensure t
  :display "Go Utility Tools"
  :commands (godef-jump
             godef-describe
             go-import-add
             go-run
             go-goto-arguments
             go-goto-docstring
             go-goto-function
             go-goto-imports
             go-goto-method-receiver
             go-goto-function-name
             go-goto-return-values))

(use-package go-ts-mode
  :ensure nil
  :mode "\\.go\\'"
  :interpreter "go"
  :hook ((go-ts-mode . symbol-overlay-mode)
         (go-ts-mode . my/disable-super-save)
         (go-ts-mode . my/go-init-logic))
  :bind (:map go-ts-mode-map
              ("<f1>"          . compile)
              ("<f5>"          . my/go-eglot-format-on-save)
              ("C-M-q"         . prog-indent-sexp)
              ("M-q"           . prog-fill-reindent-defun)
              ;; Navigation & Refactoring (Powered by go-mode)
              ("H-c a"         . go-import-add)
              ("H-c d"         . godef-describe)
              ("H-c j"         . godef-jump)
              ("H-c r"         . go-run)
              ("C-x 4 C-c C-j" . godef-jump-other-window)
              ;; "Go-to" Helpers
              ("H-c f a"       . go-goto-arguments)
              ("H-c f d"       . go-goto-docstring)
              ("H-c f f"       . go-goto-function)
              ("H-c f i"       . go-goto-imports)
              ("H-c f m"       . go-goto-method-receiver)
              ("H-c f n"       . go-goto-function-name)
              ("H-c f r"       . go-goto-return-values))

  :config
  (setq go-ts-mode-indent-offset 4)
  (defun my/go-eglot-format-on-save ()
    "Organize imports and format using Eglot."
    ;; (interactive)
    (when (eglot-managed-p)
      (ignore-errors
        (eglot-code-actions (point-min) (point-max) "source.organizeImports" t))
      (eglot-format-buffer)))

  (defun my/go-init-logic ()
    "Decide between Eglot and Apheleia based on go.mod presence."
    (let* ((proj (project-current))
           (root (when proj (project-root proj)))
           ;; Check for go.mod in project root OR current directory (for
           ;; non-project files)
           (using-eglot (and root (file-exists-p (expand-file-name "go.mod" root)))))

      (if using-eglot
          (progn
            (eglot-ensure)
            (add-hook 'before-save-hook #'my/go-eglot-format-on-save nil t))
        ;; Fallback: If no go.mod, use Apheleia if available
        (when (fboundp 'apheleia-mode)
          (apheleia-mode 1)))))

  ;; Sync environment variables if the package is available
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY"))))

(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

(use-package eglot
  :ensure nil
  :init
  (setq-default eglot-workspace-configuration
                '((:gopls . ((gofumpt . t)
                             (importShortcut . "Both")
                             (completionBudget . "100ms"))))))

(use-package apheleia
  :config
  (setf (alist-get 'go-format apheleia-formatters)
        '("golines" "--base-formatter" "gofumpt -extra"))
  (setf (alist-get 'gotmplfmt apheleia-formatters) '("gotmplfmt"))
  (setf (alist-get 'templ-fmt apheleia-formatters) '("templ" "fmt"))

  (setf (alist-get 'go-mode apheleia-mode-alist) '(goimports go-format))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) '(goimports go-format))
  ;; (setf (alist-get 'go-mode apheleia-mode-alist) 'go-format)
  ;; (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'go-format)

  (setf (alist-get 'web-mode apheleia-mode-alist) '(gotmplfmt))
  (setf (alist-get 'templ-ts-mode apheleia-mode-alist) '(templ-fmt)))

(use-package templ-ts-mode
  :mode ("\\.templ\\'" . templ-ts-mode)
  :hook ((templ-ts-mode . eglot-ensure)      ; Starts LSP automatically
         (templ-ts-mode . apheleia-mode)
         (templ-ts-mode . emmet-mode))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(templ-ts-mode . ("templ" "lsp")))))

;; (use-package gotest
;;   :bind (:map go-keymap
;;               ("C-c t f" . go-test-current-file)
;;               ("C-c t t" . go-test-current-test)
;;               ("C-c t j" . go-test-current-project)
;;               ("C-c t b" . go-test-current-benchmark)
;;               ("C-c t c" . go-test-current-coverage)
;;               ("C-c t x" . go-run)))
(use-package go-impl :ensure t)
(use-package go-tag :ensure t)
(use-package go-fill-struct :ensure t)

(provide 'init-go)

;;;;;; Go Tools
;; go install mvdan.cc/gofumpt@latest
;; go install github.com/josharian/impl@latest
;; go install github.com/fatih/gomodifytags@latest
;; go install github.com/davidrjenni/reftools/cmd/fillstruct@latest
