;;; init.el --- A Fancy and Fast Emacs Configuration.	-*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:

;;; Code:

(when (version< emacs-version "27.1")
  (error "This requires Emacs 27.1 and above!"))

;; Speed up startup

;; Defer garbage collection further back in the startup process
;; (setq gc-cons-threshold most-positive-fixnum) ;; -- test memory issues
(setq gc-cons-threshold 800000) ;; -- default

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Suppress file handlers operations at startup
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))

(when (fboundp 'native-compile-async)
  (setq comp-deferred-compilation t
        comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))


;; Load path
;; Optimize: Force "lisp" and "site-lisp" at the head to reduce the startup time
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp" "my-lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirs to `load-path'.

Don't put large files in `site-lisp' directory, eg. EAF.
Otherwise the startup will be slow."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Fonts
(set-face-attribute 'variable-pitch nil
                    :family "Iosevka Slab")
(set-frame-font "Iosevka Medium-11.25" nil t)

;; Better defaults
;; (setq initial-scratch-message nil)
(setq frame-inhibit-implied-resize nil ) ; prevents changing font, etc
                                        ; triggering a resize of the
                                        ; entire frame in non-tiling
                                        ; setups
;; Misc
(setq use-short-answers t)
(setq visible-bell t)
(setq inhibit-splash-screen t)
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
(setq confirm-kill-emacs #'y-or-n-p)

(require 'init-custom)

;; Packages
;; Without this comment Emacs25 adds (package-initialize) here
(require 'init-package)

(require 'init-clean)
(require 'init-vertico)
(require 'init-prefix)
(require 'init-core)
(require 'init-bookmark)
(require 'init-completions)
(require 'init-dired)
(require 'init-consult)
(require 'init-funcs)
(require 'init-filetree)
(require 'init-highlight)
(require 'init-simple)
(require 'init-search)
(require 'init-ibuffer)

(require 'init-ui)
(require 'init-org)
(require 'init-denote)

(require 'init-tab-bar)
(require 'init-dashboard)
(require 'init-window)
(require 'init-icon)
(require 'init-llm)
(require 'init-utils)
(require 'init-eshell)

;; Programming
(require 'init-snippets)
(require 'init-lsp)
(require 'init-langs)
(require 'init-prog)
(require 'init-racket)
(require 'init-go)
(require 'init-web)

(require 'init-vc)
(require 'init-check)
(require 'init-project)


(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Init-mini.el ends here
