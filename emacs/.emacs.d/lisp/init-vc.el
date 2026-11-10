;;; init-vc.el --- Version control configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Configuration for Ediff, Magit, Forge, and Git fringe indicators.

;;; Code:

;; Ediff
(use-package ediff
  :straight (:type built-in)
  :commands (ediff-buffers ediff-files ediff-buffers3 ediff-files3)
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-keep-variants nil)
  (ediff-make-buffers-readonly-at-startup nil)
  (ediff-merge-revisions-with-ancestor t)
  (ediff-show-clashes-only t))

;; Magit
(use-package magit
  :straight t
  :bind
  ("H-g" . magit-status)
  :custom
  (magit-define-global-key-bindings nil)
  (git-commit-summary-max-length 50)
  ;; Note: 'overlong-summary-line' omitted intentionally.
  (git-commit-style-convention-checks '(non-empty-second-line))
  (magit-diff-refine-hunk t))

;; Show TODOs in magit
(use-package magit-todos
  :straight t
  :after magit
  :custom
  (magit-todos-nice (and (executable-find "nice") t))
  (magit-todos-exclude-globs '(".git/" "vendor/" "node_modules/" "*.pb.go"))
  :config
  (magit-todos-mode 1))

;; Forge for GitHub/GitLab integration within Magit
(use-package forge
  :straight t
  :after magit)

;; Github Markdown/Org files preview
(use-package grip-mode
  :straight t
  ;; :hook ((markdown-mode org-mode) . grip-mode)
  :custom
  (grip-command 'go-grip))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :straight t
  :custom
  (diff-hl-draw-borders nil)
  (diff-hl-inline-popup--height 4)
  (fringes-outside-margins t)
  :hook
  (dired-mode . diff-hl-dired-mode)
  :config
  (global-diff-hl-mode 1)
  (global-diff-hl-show-hunk-mouse-mode 1))

;; Built-in VC customizations
(use-package vc
  :straight (:type built-in)
  :defer t
  :custom-face
  (vc-edited-state ((t (:inherit font-lock-warning-face :slant italic)))))

(provide 'init-vc)
;;; init-vc.el ends here
