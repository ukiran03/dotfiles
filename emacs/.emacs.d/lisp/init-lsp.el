;;; init-lsp.el --- summary -*- lexical-binding: t -*-


;;; Commentary:

;;; Code:

(use-package eglot
  :ensure nil
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode
                                                'lisp-mode
                                                'kdl-mode
                                                'makefile-mode
                                                'racket-mode
                                                'snippet-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure))
  :hook ((eglot-managed-mode . my/eglot-eldoc-settings))
  :config
  (setq eglot-menu-string "✦") ;; ✭►⋙
  :config
  (defun my/eglot-eldoc-settings ()
    (setq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))
  :bind (:map eglot-mode-map
              ("<f5>" . eglot-format-buffer)
              ("C-h ." . eldoc)
              ("C-c a" . eglot-code-actions)
              ("C-c r" . eglot-rename))
  :config
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  (setq eglot-autoshutdown t                   ; default: nil
        eglot-events-buffer-size 0             ; default: 2000000
        eglot-extend-to-xref t
        eglot-send-changes-idle-time 0.5))      ; default: 0.5




;; ;; Emacs LSP booster
;; (when (and emacs/>=29p (executable-find "emacs-lsp-booster"))
;;   (unless (package-installed-p 'eglot-booster)
;;     (and (fboundp #'package-vc-install)
;;          (package-vc-install "https://github.com/jdtsmith/eglot-booster")))
;;   (use-package eglot-booster
;;     :ensure nil
;;     :autoload eglot-booster-mode
;;     :init (eglot-booster-mode 1)))

;; (use-package eglot-booster
;;   :ensure (:host github
;;                  :repo "jdtsmith/eglot-booster")
;;   :if (executable-find "emacs-lsp-booster")
;;   :after eglot
;;   :init (eglot-booster-mode))



;; (use-package eglot-booster
;;   :ensure t
;;   :vc (:url "https://github.com/jdtsmith/eglot-booster")
;;   :if (executable-find "emacs-lsp-booster")
;;   :after eglot
;;   :init (eglot-booster-mode))

;; (use-package eglot-booster
;;   :ensure (:host github :repo "jdtsmith/eglot-booster")
;;   :defer)

(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
