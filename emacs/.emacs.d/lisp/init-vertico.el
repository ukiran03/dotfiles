;;; init-vertico.el --- Minibuffer Completion Configurations. -*- lexical-binding: t -*-

;;; Commentary:
;; Vertico, Marginalia, and Multiform display profiles.

;;; Code:

(require 'use-package)

(use-package compat
  :straight t)

;;;; Core Vertico Configuration
(use-package vertico
  :straight t
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :bind (:map vertico-map
              ("M-q" . vertico-quick-exit)
              ("C-q" . vertico-quick-insert)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :init
  (setq vertico-cycle t
        vertico-scroll-margin 2
        vertico-count 3
        vertico-group-format nil
        vertico-resize t))

;;;; Vertico Multiform Display Rules
(use-package vertico-multiform
  :straight nil ; Built into the vertico package
  :after vertico
  :init
  (vertico-multiform-mode 1)
  :config
  (setq vertico-multiform-categories
        '((jinx grid (vertico-grid-annotate . 20))
          (t reverse)))

  (setq vertico-multiform-commands
        '((consult-grep buffer)
          (consult-git-grep buffer)
          (consult-ripgrep buffer)
          (consult-locate buffer)
          (consult-find buffer)
          (consult-fd buffer))))

;;; Multiform Key-chord Reminders:
;; M-B -> `vertico-multiform-buffer'
;; M-F -> `vertico-multiform-flat'
;; M-G -> `vertico-multiform-grid'
;; M-R -> `vertico-multiform-reverse'
;; M-U -> `vertico-multiform-unobtrusive'
;; M-V -> `vertico-multiform-vertical'

;;;; Rich Annotations (Marginalia)
(use-package marginalia
  :straight t
  :hook (after-init . marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;;;; Orderless Completion Style
(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  ;; (setq orderless-matching-styles '(orderless-prefixes orderless-regexp))
  ;; (setq orderless-smart-case nil)
  )

(provide 'init-vertico)
;;; init-vertico.el ends here
