;;; init-vc.el --- Version control configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for Ediff, Magit, Forge, and Git fringe indicators.

;;; Code:

;; Ediff
(use-package ediff
  :straight nil
  :commands (ediff-buffers ediff-files ediff-buffers3 ediff-files3)
  :init
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t))

;; Magit
(use-package magit
  :straight t
  :bind ("H-g" . magit-status)
  :init
  (setq magit-define-global-key-bindings nil)
  :config
  (setq git-commit-summary-max-length 50)
  ;; Note: 'overlong-summary-line' omitted intentionally.
  (setq git-commit-style-convention-checks '(non-empty-second-line))
  (setq magit-diff-refine-hunk t))

;; Show TODOs in magit
(use-package magit-todos
  :straight t
  :after magit
  :config
  (setq magit-todos-nice (and (executable-find "nice") t))
  (setq magit-todos-exclude-globs '(".git/" "vendor/" "node_modules/" "*.pb.go"))
  (magit-todos-mode 1))

;; Forge for GitHub/GitLab integration within Magit
(use-package forge
  :straight t
  :after magit)

;; Github Markdown/Org files preview
(use-package grip-mode
  :straight t
  ;; :hook ((markdown-mode org-mode) . grip-mode)
  :config (setq grip-command 'go-grip))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :straight t
  :init
  (setq diff-hl-draw-borders nil)
  (setq-default diff-hl-inline-popup--height 4)
  (setq-default fringes-outside-margins t)
  :config
  (global-diff-hl-mode 1)
  (global-diff-hl-show-hunk-mouse-mode 1)
  :hook (dired-mode . diff-hl-dired-mode))

;; Built-in VC customizations
(use-package vc
  :straight nil
  :defer t
  :custom-face
  (vc-edited-state ((t (:inherit font-lock-warning-face :slant italic)))))

(provide 'init-vc)
;;; init-vc.el ends here
