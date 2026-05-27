;;; init-core.el --- Core Platform Defaults -*- lexical-binding: t -*-

;;; Shared Autoload Functions
;;;###autoload
(defun prot-common-truncate-lines-silently ()
  "Toggle line truncation without printing messages."
  (let ((inhibit-message t))
    (toggle-truncate-lines t)))

;;; Built-in Core Configurations
(use-package emacs
  :ensure nil
  :init
  ;; Window, Resize, and Frame settings
  (setq frame-inhibit-implied-resize nil)
  ;; (setq initial-scratch-message nil)
  (setq inhibit-splash-screen t)
  (setq help-window-select t)
  (setq frame-title-format
        '("%b" (:eval (let ((host (file-remote-p default-directory 'host)))
                        (when host (concat "@" host)))) " - Emacs"))

  ;; Global Operational Defaults
  (setq use-short-answers t)
  (setq visible-bell nil)
  (setq ring-bell-function #'ignore)
  (setq confirm-kill-emacs #'y-or-n-p)
  (setq delete-by-moving-to-trash t)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq set-mark-command-repeat-pop t)
  (setq kill-ring-max 200)
  (setq save-interprogram-paste-before-kill t)

  ;; Editing Typography & Spacing
  (setq-default major-mode 'text-mode)
  (setq-default c-basic-offset 4
                tab-width 4
                indent-tabs-mode nil)
  (setq adaptive-fill-regexp "[ \t]+|[ \t]*([0-9]+.\\|\\*+)[ \t]*")
  (setq adaptive-fill-first-line-regexp "^* *$")

  ;; Scrolling Architecture
  (setq scroll-preserve-screen-position 'always)

  ;; Global System Level Key Hooks
  :bind ("M-o" . other-window)
  :hook ((before-save . delete-trailing-whitespace)
         (minibuffer-setup . subword-mode)))

;;; Mode Hook Modifiers
(use-package help
  :ensure nil
  :init
  (add-to-list 'display-buffer-alist
               '("\\*Help" display-buffer-in-side-window
                 (side . right)
                 (window-width . 80))))

(use-package text-mode
  :ensure nil
  :mode "\\`\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'"
  :hook ((text-mode . turn-on-auto-fill)
         (prog-mode . (lambda ()
                        (setq-local sentence-end-double-space t))))
  :config
  (setq sentence-end-double-space nil)
  (setq sentence-end-without-period nil))

(use-package abbrev
  :ensure nil
  :blackout abbrev-mode)

;;; Core Modal Systems
(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode)
  :init
  (setq repeat-exit-key (kbd "<escape>")))

(use-package server
  :ensure nil
  :hook (after-init . server-mode))

(use-package autorevert
  :ensure nil
  :blackout global-auto-revert-mode
  :hook (after-init . global-auto-revert-mode))

(delete-selection-mode 1)

(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :config
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
  (setq electric-pair-inhibit-predicate
        #'uk/electric-pair-conservative-inhibit))

;;; Core State and History Preservation
(use-package recentf
  :ensure nil
  :bind ("C-x C-r" . recentf-open-files)
  :hook (after-init . recentf-mode)
  :init
  (setq recentf-max-saved-items 300
        recentf-auto-cleanup :never
        recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
          "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"))
  :config
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
  (add-to-list 'recentf-exclude
               (lambda (file) (file-in-directory-p file package-user-dir)))
  (when (boundp 'org-directory)
    (add-to-list 'recentf-exclude
                 (lambda (file) (file-in-directory-p file org-directory))))
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init
  (setq savehist-autosave-interval 300)
  (setq history-length 1000)
  (setq savehist-additional-variables
        '(mark-ring
          global-mark-ring search-ring
          regexp-search-ring extended-command-history)))

;;; Third-Party Navigation & Editing Extensions
(use-package beginend
  :ensure t
  :blackout beginend-global-mode
  :hook (after-init . beginend-global-mode)
  :config
  (mapc (lambda (pair) (diminish (cdr pair))) beginend-modes))

(use-package visual-regexp :ensure t)
(use-package visual-regexp-steroids :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package mwim
  :ensure t
  :bind (([remap move-beginning-of-line] . mwim-beginning)
         ([remap move-end-of-line] . mwim-end)))

(use-package vundo
  :ensure t
  :bind (("C-x /" . vundo)
         ("H-/" . vundo))
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package avy
  :ensure t
  :hook (after-init . avy-setup-default)
  :config
  (setq avy-all-windows nil
        avy-all-windows-alt t
        avy-background t
        avy-style 'pre))

(use-package goto-chg
  :ensure t
  :bind ("C-," . goto-last-change))

(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :bind ("C-h M-m" . which-key-show-major-mode)
  :config
  (setq which-key-max-description-length 30
        which-key-lighter nil
        which-key-show-remaining-keys t
        which-key-show-prefix 'bottom
        which-key-popup-type 'minibuffer
        which-key-preserve-window-configuration t
        which-key-dont-use-unicode t
        which-key-idle-delay 0.6
        which-key-idle-secondary-delay 0.2))

(use-package elisp-demos
  :ensure t
  :defer 1
  :config
  (advice-add 'describe-function-1
              :after #'elisp-demos-advice-describe-function-1))

(use-package tmr :ensure t)
(use-package suggest :ensure t)

(use-package persistent-scratch
  :disabled
  :ensure t
  :blackout
  :hook ((after-init . persistent-scratch-autosave-mode)
         (lisp-interaction-mode . persistent-scratch-mode))
  :init
  (setq persistent-scratch-backup-file-name-format "%Y-%m-%d"
        persistent-scratch-backup-directory
        (expand-file-name "data/persistent-scratch" user-emacs-directory)))

;;; Environment Shell Path Synchronizer
(use-package exec-path-from-shell
  :ensure t
  :commands exec-path-from-shell-initialize
  :custom (exec-path-from-shell-arguments '("-l"))
  :init (exec-path-from-shell-initialize))

;;; Hooks for Language Development
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-x") #'ielm)
            (local-set-key (kbd "C-c C-c") #'eval-defun)
            (local-set-key (kbd "C-c C-b") #'eval-buffer)))

;; ;;; Configuration API Definitions (Custom Defcustoms)
(defcustom ukiran-icon t
  "Display icons or not."
  :group 'centaur :type 'boolean)

(provide 'init-core)
;;; init-core.el ends here
