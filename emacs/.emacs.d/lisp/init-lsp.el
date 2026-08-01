;;; init-lsp.el --- LSP Configuration using Eglot -*- lexical-binding: t -*-

;;; Commentary:
;; Robust and high-performance LSP configuration using the built-in Eglot.

;;; Code:

(use-package eglot
  :straight nil
  :preface
  ;; Define the hook function before it is referenced in :hook
  (defun my/eglot-eldoc-settings ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))

  :hook ((eglot-managed-mode . my/eglot-eldoc-settings)
         (eglot-managed-mode . (lambda ()
                                 (setq-local project-mode-line
                                             (not
                                              (bound-and-true-p
                                               eglot--managed-mode))))))
  :bind (:map eglot-mode-map
              ("<f5>" . eglot-format-buffer)
              ("C-h ." . eldoc)
              ("C-c a" . eglot-code-actions)
              ("C-c r" . eglot-rename))
  :init
  ;; Global variables that should be set before Eglot loads
  (setq-default eglot-workspace-configuration
                '((:gopls . ((gofumpt . t)
                             (importShortcut . "Both")
                             (completionBudget . "100ms")))))
  :config
  ;; UI & UX Tweaks
  (setq eglot-menu-string "E"         ; Minimalist mode-line indicator
        eglot-report-progress t
        eglot-autoshutdown t          ; Shutdown server when last buffer closes
        eglot-extend-to-xref t
        eglot-send-changes-idle-time 0.5)

  ;; massive perf boost---don't log every event (from emacsbedrock/dev.el)
  (fset #'jsonrpc--log-event #'ignore)

  ;; Performance Optimization Enhancements
  (setq read-process-output-max (* 1024 1024) ; 1MB chunk size for reading LSP data
        eglot-events-buffer-size 0) ; Disable logging for a massive speed boost

  ;; Register Custom Language Servers
  (add-to-list 'eglot-server-programs
               '((js-mode typescript-mode) .
                 ("~/.local/share/pnpm/typescript-language-server" "--stdio"))))


;; ;; Emacs LSP booster (Uncomment if you use the binary wrapper)
;; ;; "https://github.com/jdtsmith/eglot-booster"
;; (use-package eglot-booster
;;   :straight t
;;   :after eglot
;;   :config (eglot-booster-mode 1))

(provide 'init-lsp)

;;; init-lsp.el ends here
