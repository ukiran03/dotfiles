;;; init-core.el --- Core Platform Defaults -*- lexical-binding: t -*-

;;; Built-in Core Configurations
(use-package emacs
  :straight (:type built-in)
  :custom
  ;; Window, Resize, and Frame settings
  (frame-inhibit-implied-resize nil)
  (initial-scratch-message nil)
  (inhibit-startup-screen t)
  (help-window-select t)
  (frame-title-format
   '("%b" (:eval (let ((host (file-remote-p default-directory 'host)))
                   (when host (concat "@" host)))) " - Emacs"))

  ;; Global Operational Defaults
  (use-short-answers t)
  (visible-bell t)
  (ring-bell-function nil)
  (confirm-kill-emacs #'y-or-n-p)
  (delete-by-moving-to-trash t)
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (set-mark-command-repeat-pop t)
  (kill-ring-max 200)
  (save-interprogram-paste-before-kill t)

  ;; Editing Typography & Spacing
  (major-mode 'text-mode)
  (c-basic-offset 4)
  (tab-width 4)
  (indent-tabs-mode nil)
  (adaptive-fill-regexp "[ \t]+|[ \t]*([0-9]+.\\|\\*+)[ \t]*")
  (adaptive-fill-first-line-regexp "^* *$")

  ;; Scrolling Architecture
  (scroll-preserve-screen-position 'always)

  :bind
  ("M-o" . other-window)

  :hook
  ((before-save . delete-trailing-whitespace)
   (minibuffer-setup . subword-mode)))

(use-package help
  :straight (:type built-in)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Help" display-buffer-in-side-window
                 (side . right)
                 (window-width . 80))))

(use-package text-mode
  :straight (:type built-in)
  :mode "\\`\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'"
  :custom
  (sentence-end-double-space nil)
  (sentence-end-without-period nil)
  :hook
  ((text-mode . turn-on-auto-fill)
   (prog-mode . (lambda ()
                  (setq-local sentence-end-double-space t)))))

(use-package abbrev
  :straight (:type built-in))

;;; Core Modal Systems
(use-package repeat
  :straight (:type built-in)
  :hook (after-init . repeat-mode)
  :custom
  (repeat-exit-key (kbd "<escape>")))

(use-package server
  :straight (:type built-in)
  :hook (after-init . server-mode))

(use-package autorevert
  :straight (:type built-in)
  :hook (after-init . global-auto-revert-mode))

(use-package delsel
  :straight (:type built-in)
  :hook (after-init . delete-selection-mode))

(use-package elec-pair
  :straight (:type built-in)
  :hook (after-init . electric-pair-mode)
  :preface
  (defun uk/electric-pair-conservative-inhibit (char)
    "Ensures single quotes or pairs are not added errantly around words.
from `r/emacs/comments/1hwf46n/comment/m63mddk'"
    (or (eq char (char-after))
        (and (eq char (char-before))
             (eq char (char-before (1- (point)))))
        (eq (char-syntax (following-char)) ?w)
        (and (eq (char-syntax (char-before (1- (point)))) ?w)
             (eq (preceding-char) char)
             (not (eq (char-syntax (preceding-char)) ?\()))))
  :custom
  (electric-pair-inhibit-predicate #'uk/electric-pair-conservative-inhibit))

;;; Core State and History Preservation
(use-package recentf
  :straight (:type built-in)
  :bind ("C-x C-r" . recentf-open-files)
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 300)
  (recentf-auto-cleanup :never)
  (recentf-exclude
   '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
     "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
     "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
     "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"))
  :config
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
  (when (and (boundp 'package-user-dir) package-user-dir)
    (add-to-list 'recentf-exclude
                 (lambda (file) (file-in-directory-p file package-user-dir))))
  (when (boundp 'org-directory)
    (add-to-list 'recentf-exclude
                 (lambda (file) (file-in-directory-p file org-directory))))
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

(use-package saveplace
  :straight (:type built-in)
  :hook (after-init . save-place-mode))

(use-package savehist
  :straight (:type built-in)
  :hook (after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 300)
  (history-length 1000)
  (savehist-additional-variables
   '(mark-ring
     global-mark-ring search-ring
     regexp-search-ring extended-command-history)))

;;; Third-Party Navigation & Editing Extensions
(use-package beginend
  :straight t
  :hook (after-init . beginend-global-mode))

(use-package visual-regexp :straight t)
(use-package visual-regexp-steroids :straight t)

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package mwim
  :straight t
  :bind (([remap move-beginning-of-line] . mwim-beginning)
         ([remap move-end-of-line] . mwim-end)))

(use-package vundo
  :straight t
  :bind (("C-x /" . vundo)
         ("H-/" . vundo))
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package avy
  :straight t
  :hook (after-init . avy-setup-default)
  :custom
  (avy-all-windows nil)
  (avy-all-windows-alt t)
  (avy-background t)
  (avy-style 'pre))

(use-package goto-chg
  :straight t
  :bind ("C-," . goto-last-change))

(use-package easy-kill
  :straight t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(use-package which-key
  :straight t
  :hook (after-init . which-key-mode)
  :bind ("C-h M-m" . which-key-show-major-mode)
  :custom
  (which-key-max-description-length 30)
  (which-key-lighter nil)
  (which-key-show-remaining-keys t)
  (which-key-show-prefix 'bottom)
  (which-key-popup-type 'minibuffer)
  (which-key-preserve-window-configuration t)
  (which-key-dont-use-unicode t)
  (which-key-idle-delay 0.6)
  (which-key-idle-secondary-delay 0.2))

(use-package elisp-demos
  :straight t
  :defer 1
  :config
  (advice-add 'describe-function-1
              :after #'elisp-demos-advice-describe-function-1))

(use-package tmr :straight t)
(use-package suggest :straight t)

(use-package persistent-scratch
  :disabled
  :straight t
  :hook ((after-init . persistent-scratch-autosave-mode)
         (lisp-interaction-mode . persistent-scratch-mode))
  :custom
  (persistent-scratch-backup-file-name-format "%Y-%m-%d")
  (persistent-scratch-backup-directory
   (expand-file-name "data/persistent-scratch" user-emacs-directory)))

;;; Environment Shell Path Synchronizer
(use-package exec-path-from-shell
  :straight t
  :commands exec-path-from-shell-initialize
  :custom (exec-path-from-shell-arguments '("-l"))
  :init (exec-path-from-shell-initialize))

;;; Hooks for Language Development
(use-package emacs-lisp-mode
  :straight (:type built-in)
  :hook
  (emacs-lisp-mode . (lambda ()
                       (local-set-key (kbd "C-c C-x") #'ielm)
                       (local-set-key (kbd "C-c C-c") #'eval-defun)
                       (local-set-key (kbd "C-c C-b") #'eval-buffer))))

;;; Configuration API Definitions (Custom Defcustoms)
(defcustom ukiran-icon t
  "Display icons or not."
  :group 'centaur :type 'boolean)

(provide 'init-core)
;;; init-core.el ends here
