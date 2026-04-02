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

(defun my/golang-init-eglot ()
  "Start eglot only if a go.mod exists in the project root."
  (let* ((proj (project-current))
         (root (when proj (project-root proj))))
    (when (and root (file-exists-p (expand-file-name "go.mod" root)))
      (eglot-ensure))))

;; (defun my/golang-init-eglot ()
;;   "Start Eglot if go.mod exists; otherwise, fix Apheleia for scratch files."
;;   (let* ((proj (project-current))
;;          (root (when proj (project-root proj))))
;;     (if (and root (file-exists-p (expand-file-name "go.mod" root)))
;;         ;; CASE 1: Project found
;;         (eglot-ensure)

;;       ;; CASE 2: Scratch file (No go.mod)
;;       ;; We manually add goimports back to the chain for THIS buffer only.
;;       (setq-local apheleia-mode-alist
;;                   (append '((go-ts-mode . (goimports uk-go-fmt)))
;;                           apheleia-mode-alist)))))

;; (defun my/go-organize-imports-on-save ()
;;   "Safely organize imports via Eglot for Go buffers."
;;   (when (derived-mode-p 'go-ts-mode)
;;     (add-hook 'before-save-hook
;;               (lambda ()
;;                 (interactive)
;;                 (ignore-errors (eglot-code-action-organize-imports
;;                                 (point-min) (point-max)))) nil t)

;;     ;; ;; 2. The Apheleia Logic (Buffer-local)
;;     ;; ;; We make this change buffer-local so it doesn't affect other Go files
;;     ;; (setq-local apheleia-mode-alist
;;     ;;             (copy-alist apheleia-mode-alist))
;;     ;; (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'go-format)
;;     ))

(use-package go-ts-mode
  :ensure nil
  :mode "\\.go\\'"
  :interpreter "go"
  :hook ((go-ts-mode . symbol-overlay-mode)
         (go-ts-mode . apheleia-mode) ; TODO:
         (go-ts-mode . my/disable-super-save)
         (go-ts-mode . my/golang-init-eglot)
         ;; Attach the import-organizer only when Eglot is actually active
         ;; (eglot-managed-mode-hook . my/go-organize-imports-on-save)
         )

  :bind (:map go-ts-mode-map
              ("<f1>"          . compile)
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

  ;; Sync environment variables if the package is available
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY"))))

(add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

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

(use-package go-impl
  :ensure t)

(use-package go-tag
  :ensure t)

(use-package go-fill-struct)

(provide 'init-go)

;;;;;; Go Tools
;; go install mvdan.cc/gofumpt@latest
;; go install github.com/josharian/impl@latest
;; go install github.com/fatih/gomodifytags@latest
;; go install github.com/davidrjenni/reftools/cmd/fillstruct@latest
