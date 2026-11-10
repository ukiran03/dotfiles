;;; init-completions.el --- Modern Completion Engine -*- lexical-binding: t -*-

;;; Core Emacs Minibuffer & UI Configuration
(use-package emacs
  :straight (:type built-in)
  :custom
  ;; Minibuffer behavior
  (enable-recursive-minibuffers t)
  (completion-cycle-threshold 3)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (tab-always-indent 'complete)

  ;; Prevent cursor from entering the prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Global Search and UI Adjustments
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (case-fold-search t)
  (resize-mini-windows t)
  (read-answer-short t)
  (echo-keystrokes 0.25)
  (kill-ring-max 60)

  ;; Built-in Minibuffer Styling & UI
  (completions-detailed t)
  (completions-max-height 6)
  (completions-format 'one-column)
  (completion-show-help nil)
  (completion-auto-help 'always)
  (completion-auto-select nil)
  (completion-show-inline-help nil)
  (completions-header-format (propertize "%s candidates:\n" 'face 'bold-italic))
  (completions-highlight-face 'completions-highlight)
  (minibuffer-completion-auto-choose t)

  ;; Matching Styles & Categories
  (completion-styles '(basic substring initials flex orderless))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles . (basic partial-completion orderless)))
     (bookmark (styles . (basic substring)))
     (library (styles . (basic substring)))
     (embark-keybinding (styles . (basic substring)))
     (imenu (styles . (basic substring orderless)))
     (consult-location (styles . (basic substring orderless)))
     (kill-ring (styles . (emacs22 orderless)))
     (eglot (styles . (emacs22 substring orderless))))))

;;; Minibuffer Visual Enhancements
(use-package mb-depth
  :straight (:type built-in)
  :hook (after-init . minibuffer-depth-indicate-mode)
  :custom
  (read-minibuffer-restore-windows nil))

(use-package minibuf-eldef
  :straight (:type built-in)
  :hook (after-init . minibuffer-electric-default-mode)
  :custom
  (minibuffer-default-prompt-format " [%s]"))

;;; Session History Preservation
(use-package savehist
  :straight (:type built-in)
  :hook (after-init . savehist-mode)
  :custom
  (history-length 100)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  :config
  (add-to-list 'savehist-additional-variables 'kill-ring))

(use-package rfn-eshadow
  :straight (:type built-in)
  :hook (minibuffer-setup . cursor-intangible-mode)
  :config
  (file-name-shadow-mode 1))

;;; Context Actions & Exporting (Embark)
(use-package embark
  :straight t
  :bind
  (:map minibuffer-local-map
        ("C-c C-c" . embark-collect)
        ("C-c C-." . embark-act)
        ("C-c C-e" . embark-export)))

(use-package embark-consult
  :straight t
  :after (embark consult))

;;; In-Buffer Auto-Completion (Corfu)
(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  (corfu-min-width 20)
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind
  (:map corfu-map
        ("M-SPC" . corfu-insert-separator)
        ("TAB"   . corfu-next)
        ("S-TAB" . corfu-previous)
        ("C-n"   . corfu-next)
        ("C-p"   . corfu-previous)
        ("S-<return>" . corfu-insert)
        ("RET"   . nil))
  :hook
  ((after-init . global-corfu-mode)
   (global-corfu-mode . corfu-echo-mode))
  :config
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;;; Completion At Point Extensions (Cape)
(use-package cape
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(provide 'init-completions)
;;; init-completions.el ends here
