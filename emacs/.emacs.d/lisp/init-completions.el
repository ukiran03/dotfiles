;;; init-completions.el --- Modern Completion Engine -*- lexical-binding: t -*-

;;; Context Actions & Exporting (Embark)
(use-package embark
  :ensure t
  :bind (:map minibuffer-local-map
              ("C-c C-c" . embark-collect)
              ("C-c C-." . embark-act)
              ("C-c C-e" . embark-export)))

(use-package embark-consult
  :ensure t
  :after (embark consult))

;;; Core Emacs Minibuffer & UI Configuration
(use-package emacs
  :ensure nil
  :init
  ;; Minibuffer behavior
  (setq enable-recursive-minibuffers t)
  (setq completion-cycle-threshold 3)
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  (setq tab-always-indent 'complete)

  ;; Prevent cursor from entering the prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Global Search and UI Adjustments
  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq-default case-fold-search t)
  (setq resize-mini-windows t)
  (setq read-answer-short t)
  (setq echo-keystrokes 0.25)
  (setq kill-ring-max 60))

;;; Minibuffer Visual Enhancements
(use-package mb-depth
  :ensure nil
  :hook (after-init . minibuffer-depth-indicate-mode)
  :config
  (setq read-minibuffer-restore-windows nil))

(use-package minibuf-eldef
  :ensure nil
  :hook (after-init . minibuffer-electric-default-mode)
  :config
  (setq minibuffer-default-prompt-format " [%s]"))

;;; Built-in Completion Styling & UI
(use-package minibuffer
  :ensure nil
  :demand t
  :config
  ;; Layout and UI
  (setq completions-detailed t)
  (setq completions-max-height 6)
  (setq completions-format 'one-column)
  (setq completion-show-help nil)
  (setq completion-auto-help 'always)
  (setq completion-auto-select nil)
  (setq completion-show-inline-help nil)
  (setq completions-header-format (propertize "%s candidates:\n" 'face 'bold-italic))
  (setq completions-highlight-face 'completions-highlight)
  (setq minibuffer-completion-auto-choose t)

  ;; Matching Styles (Fallback options if Orderless is absent)
  (setq completion-styles '(basic substring initials flex orderless))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides
        '((file (styles . (basic partial-completion orderless)))
          (bookmark (styles . (basic substring)))
          (library (styles . (basic substring)))
          (embark-keybinding (styles . (basic substring)))
          (imenu (styles . (basic substring orderless)))
          (consult-location (styles . (basic substring orderless)))
          (kill-ring (styles . (emacs22 orderless)))
          (eglot (styles . (emacs22 substring orderless))))))

;;; Session History Preservation
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq history-length 100)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-to-list 'savehist-additional-variables 'kill-ring))

(use-package rfn-eshadow
  :ensure nil
  :hook (minibuffer-setup . cursor-intangible-mode)
  :config
  (file-name-shadow-mode 1))

;;; In-Buffer Auto-Completion (Corfu)
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  (corfu-min-width 20)
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind (:map corfu-map
              ("M-SPC" . corfu-insert-separator)
              ("TAB"   . corfu-next)
              ("S-TAB" . corfu-previous)
              ("C-n"   . corfu-next)
              ("C-p"   . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"   . nil))
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-echo-mode))
  :config
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;;; Completion At Point Extensions (Cape)
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(provide 'init-completions)
;;; init-completions.el ends here
