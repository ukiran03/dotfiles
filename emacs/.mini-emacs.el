;;; Commentary:
;;
;; Minimal configurations for debugging purpose.
;;

;;; Code:

;; Explicitly set the preferred coding systems to avoid annoying prompts
(prefer-coding-system 'utf-8)

;;; Core Settings & Better Defaults
(setq use-short-answers t
      visible-bell t
      delete-by-moving-to-trash t
      set-mark-command-repeat-pop t
      uniquify-buffer-name-style 'post-forward-angle-brackets
      kill-ring-max 200
      save-interprogram-paste-before-kill t
      scroll-preserve-screen-position 'always
      help-window-select t)

;; Backup and Auto-save settings
(setq make-backup-files t
      auto-save-default t
      create-lockfiles nil
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Sentence formatting
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

;; Tabs and Spaces (Never use TABs)
(setq-default major-mode 'text-mode
              c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

;;; UI & Appearance
(set-frame-font "Iosevka Medium-14" nil t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(show-paren-mode 1)
(delete-selection-mode 1)

;; Frame Title Format
(setq frame-title-format
      '("%b"
        (:eval (let ((host (file-remote-p default-directory 'host)))
                 (when host
                   (concat "@" host))))
        " - Emacs"))

;;; Built-in Packages & Hooks

(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode)
  :init
  (setq repeat-exit-key (kbd "<escape>")))

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init
  (setq history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 300))

(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package text-mode
  :ensure nil
  :mode "\\`\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'"
  :hook ((text-mode . turn-on-auto-fill)
         (prog-mode . (lambda () (setq-local sentence-end-double-space t)))))

(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'minibuffer-setup-hook #'subword-mode)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;; Completion Setup (Fido / Minibuffer)
(fido-vertical-mode 1)

(setopt read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-auto-help t
        completion-auto-select 'second-tab
        completion-cycle-threshold 3
        completions-format 'vertical
        completions-sort 'historical
        completions-max-height 10
        completion-show-help nil
        completion-styles '(basic partial-completion initials))

;;; Dired Configuration
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode)
              ("_" . dired-create-empty-file))
  :config
  (setq dired-auto-revert-buffer #'dired-directory-changed-p
        dired-dwim-target t
        dired-free-space 'first
        dired-make-directory-clickable t
        dired-mouse-drag-files t
        delete-by-moving-to-trash t
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso"))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;;; Custom Functions & Keybindings
(defun fido-recentf-open ()
  "Use `completing-read' to find a recent file."
  (interactive)
  (if (find-file (completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defun revert-current-buffer ()
  "Revert the current buffer."
  (interactive)
  (message "Revert this buffer")
  (text-scale-set 0)
  (widen)
  (revert-buffer t t))

(use-package simple
  :ensure nil
  :config
  (defun uk-simple-copy-line ()
    "Copy the current line to the `kill-ring'."
    (interactive)
    (copy-region-as-kill (line-beginning-position) (line-end-position))
    (pulse-momentary-highlight-region (line-beginning-position) (line-end-position)))
  (global-set-key (kbd "C-S-w") #'uk-simple-copy-line))

;; Global Keybindings
(global-set-key (kbd "C-x C-r") #'fido-recentf-open)
(global-set-key (kbd "<f5>") #'revert-current-buffer)
(global-set-key (kbd "M-o") #'other-window)

;; Mode-specific Keybindings
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-x") #'ielm)
            (local-set-key (kbd "C-c C-c") #'eval-defun)
            (local-set-key (kbd "C-c C-b") #'eval-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Init-mini.el ends here
