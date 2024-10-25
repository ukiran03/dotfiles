(use-package emacs
  :ensure nil
  :config
  (setq help-window-select t))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq frame-title-format '("Emacs - %b")
      icon-title-format frame-title-format)

(use-package repeat
  :ensure nil
  :init (repeat-mode))

(use-package server
  :ensure nil
  :init (server-mode))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :init (global-auto-revert-mode))


;; Redefine M-< and M-> for some modes
(use-package beginend
  :diminish beginend-global-mode
  :hook (after-init . beginend-global-mode)
  :config (mapc (lambda (pair)
                  (diminish (cdr pair)))
                beginend-modes))


(use-package visual-regexp
  :config
  (use-package visual-regexp-steroids))

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
  :bind ("C-x /" . vundo)
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
(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))

(setq kill-ring-max 200)

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Interactively insert and edit items from kill-ring
(use-package browse-kill-ring
  :bind ("C-c k" . browse-kill-ring)
  :hook (after-init . browse-kill-ring-default-keybindings)
  :init (setq browse-kill-ring-separator "────────────────"
              browse-kill-ring-separator-face 'shadow))

;; History
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
  (add-to-list 'recentf-exclude
           (concat (getenv "HOME") "/Org/*"))  ;;; TODO: Read the
                           ;;; `Regexp'
                           ;;; chapter in
                           ;;; Emacs manual

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

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)


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

(use-package gdb-mi
  :ensure nil
  :config
  (setq gdb-many-windows t)
  (setq gdb-show-main t))

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
        which-key-show-remaining-keys t)
  :bind ("C-h M-m" . which-key-show-major-mode))

(use-package elisp-demos
  :defer 1
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))

;; Interactive macro expander
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
         ("C-c e" . macrostep-expand)
         :map lisp-interaction-mode-map
         ("C-c e" . macrostep-expand)))

(use-package tmr)

;; Persistent the scratch buffer
;; <https://github.com/Fanael/persistent-scratch>
;; <https://umarahmad.xyz/blog/quick-scratch-buffers/>
(use-package persistent-scratch
  :diminish
  ;; :bind (:map persistent-scratch-mode-map
  ;;        ([remap kill-buffer] . (lambda (&rest _)
  ;;                                 (interactive)
  ;;                                 (user-error "Scratch buffer cannot be killed")))
  ;;        ([remap revert-buffer] . persistent-scratch-restore)
  ;;        ([remap revert-this-buffer] . persistent-scratch-restore))
  :hook ((after-init . persistent-scratch-autosave-mode)
         (lisp-interaction-mode . persistent-scratch-mode))
  :init (setq persistent-scratch-backup-file-name-format "%Y-%m-%d"
              persistent-scratch-backup-directory
              (expand-file-name "persistent-scratch" user-emacs-directory)))

(use-package fzf
  ;; :bind
  ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

(provide 'init-core)
