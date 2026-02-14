;;; Commentary:
;;
;; Minimal configurations for debugging purpose.
;;

;;; Code:

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Better defaults
;; (setq initial-scratch-message nil)
;; (setq inhibit-splash-screen t)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again
;; (setq kill-whole-line t)                   ; Kill line including '\n'

(setq-default major-mode 'text-mode)

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

;; (set-frame-font "Iosevka Medium-11.25" nil t)
(set-frame-font "Iosevka Medium-14" nil t)

;; (menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; (global-display-line-numbers-mode 1)

;; (global-hl-line-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(electric-pair-mode 1)
(repeat-mode 1)

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'minibuffer-setup-hook #'subword-mode)

;; Completion
;; (fido-vertical-mode 1)

(setq completion-auto-help 'visible)

(defun fido-recentf-open ()
  "Use `completing-read' to find a recent file."
  (interactive)
  (if (find-file (completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
(global-set-key (kbd "C-x C-r") 'fido-recentf-open)

(defun revert-current-buffer ()
  "Revert the current buffer."
  (interactive)
  (message "Revert this buffer")
  (text-scale-set 0)
  (widen)
  (revert-buffer t t))
(global-set-key (kbd "<f5>") #'revert-current-buffer)

(use-package simple
  :ensure nil
  :config
  (defun uk-simple-copy-line ()
    "Copy the current line to the `kill-ring'."
    (interactive)
    (copy-region-as-kill (line-beginning-position) (line-end-position))
    (pulse-momentary-highlight-region (line-beginning-position) (line-end-position)))
  (global-set-key (kbd "C-S-w") 'uk-simple-copy-line))

(global-set-key (kbd "M-o") 'other-window)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-x") #'ielm)
            (local-set-key (kbd "C-c C-c") #'eval-defun)
            (local-set-key (kbd "C-c C-b") #'eval-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Init-mini.el ends here

;;; Completions Configuration

(setopt read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t

        ;; This *may* need to be set to 'always just so that you don't
        ;; miss other possible good completions that match the input
        ;; string.
        completion-auto-help t

        ;; Move focus to the completions window after hitting tab
        ;; twice.
        completion-auto-select 'second-tab

        ;; If there are 3 or less completion candidates, don't pop up
        ;; a window, just cycle through them.
        completion-cycle-threshold 3

        ;; Cycle through completion options vertically, not
        ;; horizontally.
        completions-format 'vertical

        ;; Sort recently used completions first.
        completions-sort 'historical

        ;; Only show up to 10 lines in the completions window.
        completions-max-height 10

        ;; Don't show the unneeded help string at the top of the
        ;; completions buffer.
        completion-show-help nil

        ;; Add the 'initials' completion style to the default list.
        completion-styles '(basic partial-completion initials))

;; Consider setting `completion-category-overrides` to customize
;; individual minibuffer prompts with different completion-styles,
;; sorting, etc.

;; Activate the theme
;; (load-theme 'tango t)
;; (load-theme 'tango-dark t)

;; -----------------------
;; Dired
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode)
              ("_" . dired-create-empty-file))
  :config
  (setq dired-auto-revert-buffer #'dired-directory-changed-p)

  ;; Guess a default target directory
  (setq dired-dwim-target t)

  (setq dired-free-space 'first) ; Emacs 29.01
  (setq dired-make-directory-clickable t); Emacs 29.01
  (setq dired-mouse-drag-files t); Emacs 29.01

  (setq delete-by-moving-to-trash t)

  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  ;; (add-hook 'dired-mode-hook #'hl-line-mode)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  ;; Show directory first
  (setq dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")

  ;; Extra Dired functionality
  )

;; Core

(setq use-short-answers t)
(setq visible-bell t)
;; (setq inhibit-splash-screen t)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again
;; (setq kill-whole-line t)                   ; Kill line including '\n'

(setq make-backup-files t)
(setq auto-save-default t)
;; check init-clean.el

;; Move this in its own thing
;; (setq
;;  create-lockfiles nil
;;  delete-old-versions t
;;  kept-new-versions 6
;;  kept-old-versions 2
;;  version-control t)

(setq-default major-mode 'text-mode)

;; Quitting emacs via `C-x C-c` or the GUI 'X' button
;; (setq confirm-kill-emacs #'y-or-n-p)

(use-package emacs
  :ensure nil
  :config
  (setq help-window-select t))

;; C-v and M-v don't undo each other, because the point position isn't
;; preservered. Fix that.
(setq scroll-preserve-screen-position 'always)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; (setq frame-title-format '("Emacs - %b")
;;       icon-title-format frame-title-format)

(setq frame-title-format
      '("%b"
        (:eval (let ((host (file-remote-p default-directory 'host)))
                 (when host
                   (concat "@" host))))
        " - Emacs"))

(use-package text-mode
  :ensure nil
  :mode "\\`\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'"
  :hook
  ((text-mode . turn-on-auto-fill)
   (prog-mode . (lambda () (setq-local sentence-end-double-space t))))
  :config
  (setq sentence-end-double-space nil)
  (setq sentence-end-without-period nil))

(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode)
  :init
  (setq repeat-exit-key (kbd "<escape>")))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))


(setq kill-ring-max 200)

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; History
;;; TODO: Read the `Regexp' chapter in Emacs manual
;; (use-package recentf
;;   :bind (("C-x C-r" . recentf-open-files))
;;   :hook (after-init . recentf-mode)
;;   :init (setq recentf-max-saved-items 300
;;               recentf-auto-cleanup :never
;;               recentf-exclude
;;               '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
;;                 "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
;;                 "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
;;                 "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
;;                 (lambda (file) (file-in-directory-p file package-user-dir))))
;;   (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
;;   (add-to-list 'recentf-exclude
;;                (concat (getenv "HOME") "/Documents/org/*"))
;;   :config
;;   (push (expand-file-name recentf-save-file) recentf-exclude)
;;   (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

;; Save place
(use-package saveplace
  :hook (after-init . save-place-mode))
(use-package savehist
  :hook (after-init . savehist-mode)
  :init (setq history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

;; Basic modes
(delete-selection-mode 1)

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))
(add-hook 'minibuffer-setup-hook #'subword-mode)

;; (set-face-attribute 'org-modern-symbol nil :family "Iosevka" :height 110)
;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

(global-set-key (kbd "M-o") 'other-window)

(use-package abbrev
  :ensure nil)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-x") #'ielm)
            (local-set-key (kbd "C-c C-c") #'eval-defun)
            (local-set-key (kbd "C-c C-b") #'eval-buffer)))
