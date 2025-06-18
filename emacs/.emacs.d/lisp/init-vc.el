;;; init-vc.el --- summary -*- lexical-binding: t -*-


;;; Commentary:

;;; Code:

;;;; `ediff'
(use-package ediff
  :ensure nil
  :commands (ediff-buffers ediff-files ediff-buffers3 ediff-files3)
  :init
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (setq ediff-keep-variants nil)
  (setq ediff-make-buffers-readonly-at-startup nil)
  (setq ediff-merge-revisions-with-ancestor t)
  (setq ediff-show-clashes-only t))

(use-package magit :ensure t)
(use-package forge :ensure nil)

(use-package diff-hl)

(use-package magit
  :ensure t
  :bind ("H-g" . magit-status)
  :init
  (setq magit-define-global-key-bindings nil)
  ;; (setq magit-section-visibility-indicator '("⮧"))
  :config
  (setq git-commit-summary-max-length 50)
  ;; NOTE 2023-01-24: I used to also include `overlong-summary-line'
  ;; in this list, but I realised I do not need it.  My summaries are
  ;; always in check.  When I exceed the limit, it is for a good
  ;; reason.
  (setq git-commit-style-convention-checks '(non-empty-second-line))

  (setq magit-diff-refine-hunk t))


;; Highlight uncommitted changes using VC
(use-package diff-hl
  :init
  (setq diff-hl-draw-borders nil)
  (setq-default diff-hl-inline-popup--height 4)
  :hook ((after-init . global-diff-hl-mode)
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  ;;   ;; Highlight on-the-fly
  ;;   (diff-hl-flydiff-mode 1)

  ;;   ;; Set fringe style
  (setq-default fringes-outside-margins t))




(provide 'init-vc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vc.el ends here
