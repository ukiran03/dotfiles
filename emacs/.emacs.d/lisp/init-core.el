;;; init-core.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package emacs
  :ensure nil
  :config
  (setq help-window-select t))

;; C-v and M-v don't undo each other, because the point position isn't
;; preservered. Fix that.
(setq scroll-preserve-screen-position 'always)

(use-package help
  :ensure nil
  :init
  (add-to-list 'display-buffer-alist
               '("\\*Help" display-buffer-in-side-window
                 (side . right)
                 (window-width . 80))))

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

(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Redefine M-< and M-> for some modes
(use-package beginend
  :diminish beginend-global-mode
  :hook (after-init . beginend-global-mode)
  :config (mapc (lambda (pair)
                  (diminish (cdr pair)))
                beginend-modes))

(use-package visual-regexp)
(use-package visual-regexp-steroids)

;; Increase selected region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Smartly select region, rectangle, multi cursors
(use-package smart-region               ; `eating' -- `C-SPC C-SPC' `mark-deactivated'
  :disabled
  :hook (after-init . smart-region-on))

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning)
         ([remap move-end-of-line] . mwim-end)))

(use-package vundo
  :bind (("C-x /" . vundo)
         ("H-/" . vundo))
  :config (setq vundo-glyph-alist vundo-unicode-symbols))

;; Jump to things in Emacs tree-style
(use-package avy
  :bind (("C-:"   . avy-goto-char-timer)
         ("C-;"   . avy-goto-char-2)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1))
  :hook (after-init . avy-setup-default)
  :config (setq avy-all-windows nil
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

;; Goto last change
(use-package goto-chg
  :bind ("C-," . goto-last-change))

;; Kill & Mark things easily
;; <https://github.com/leoliu/easy-kill>
;;
;; M-w w: save word at point
;; M-w s: save sexp at point
;; M-w l: save list at point (enclosing sexp)
;; M-w d: save defun at point
;; M-w D: save current defun name
;; M-w f: save file at point
;; M-w b: save buffer-file-name or default-directory.
                                        ; - changes the
                                        ; kill to the directory name,
                                        ; + to full name and 0 to
                                        ; basename.
;; C-h f easy-kill
(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(setq kill-ring-max 200)

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; History
;;; TODO: Read the `Regexp' chapter in Emacs manual
(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
              recentf-auto-cleanup :never
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
  (add-to-list 'recentf-exclude
               (concat (getenv "HOME") "/Documents/org/*"))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name))

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
  :ensure nil
  :diminish abbrev-mode)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-x") #'ielm)
            (local-set-key (kbd "C-c C-c") #'eval-defun)
            (local-set-key (kbd "C-c C-b") #'eval-buffer)))

(use-package which-key
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-max-description-length 30
        which-key-lighter nil
        which-key-show-remaining-keys t
        which-key-show-prefix 'top
        which-key-popup-type 'minibuffer
        which-key-preserve-window-configuration t
        which-key-max-description-length 30
        which-key-dont-use-unicode t
        which-key-idle-delay 0.6
        which-key-idle-secondary-delay 0.2)
  :bind ("C-h M-m" . which-key-show-major-mode))

(use-package elisp-demos
  :defer 1
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))

(use-package tmr)

;; Persistent the scratch buffer
;; <https://github.com/Fanael/persistent-scratch>
;; <https://umarahmad.xyz/blog/quick-scratch-buffers/>
(use-package persistent-scratch
  :diminish
  :hook ((after-init . persistent-scratch-autosave-mode)
         (lisp-interaction-mode . persistent-scratch-mode))
  :init (setq persistent-scratch-backup-file-name-format "%Y-%m-%d"
              persistent-scratch-backup-directory
              (expand-file-name "data/persistent-scratch" user-emacs-directory)))

;; https://github.com/Wilfred/suggest.el
;; discover elisp functions that do what you want
(use-package suggest)

(use-package affe
  :ensure t
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key "M-."))

(use-package no-littering)

(use-package super-save
  ;; :disabled
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(provide 'init-core)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-core.el ends here
