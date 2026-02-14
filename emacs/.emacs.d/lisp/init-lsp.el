;;; init-lsp.el --- summary -*- lexical-binding: t -*-


;;; Commentary:

;;; Code:

(use-package eglot
  :ensure nil
  :hook ((eglot-managed-mode . my/eglot-eldoc-settings)
         (eglot-managed-mode . (lambda ()
                                 (setq-local project-mode-line
                                             (not
                                              (bound-and-true-p
                                               eglot--managed-mode)))))
         ;; (prog-mode . (lambda ()
         ;;                (unless (derived-mode-p
         ;;                         'emacs-lisp-mode 'lisp-mode
         ;;                         'makefile-mode 'snippet-mode
         ;;                         'ron-mode)
         ;;                  (eglot-ensure))))
         ;; ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure)
         )
  :bind (:map eglot-mode-map
              ("<f5>" . eglot-format-buffer)
              ("C-h ." . eldoc)
              ("C-c a" . eglot-code-actions)
              ("C-c r" . eglot-rename))
  :config
  (setq eglot-menu-string "E" ;; ✭►⋙
        eglot-report-progress t
        read-process-output-max (* 1024 1024) ; 1MB
        eglot-autoshutdown t                   ; default: nil
        eglot-events-buffer-size 0             ; default: 2000000
        eglot-extend-to-xref t
        eglot-send-changes-idle-time 0.5)      ; default: 0.5
  (defun my/eglot-eldoc-settings ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly)))

(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs
               '((js-mode . ("~/.local/share/pnpm/typescript-language-server" "--stdio"))
                 (typescript-mode . ("~/.local/share/pnpm/typescript-language-server" "--stdio"))))
  )



;; ;; Emacs LSP booster
;; "https://github.com/jdtsmith/eglot-booster"
;;   (use-package eglot-booster)

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
