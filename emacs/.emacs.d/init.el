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
  ;; Temporarily suppress file-handler processing to speed up startup
  (let ((default-handlers file-name-handler-alist))
    (setq file-name-handler-alist nil)
    ;; Recover handlers after startup
    (add-hook 'emacs-startup-hook
              (lambda ()
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist default-handlers))))
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

;; Add subdirectories inside "site-lisp" to `load-path`
(defun add-subdirs-to-load-path (&rest _)
  "Recursively add subdirectories in `site-lisp` to `load-path`.

Avoid placing large files like EAF in `site-lisp` to prevent slow startup."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; Fonts
(set-face-attribute 'variable-pitch nil
                    :family "Iosevka Slab")
(set-frame-font "Iosevka Medium-11.25" nil t)

(require 'init-custom)
(require 'init-funcs)
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
(require 'init-mail)
(require 'init-vc)
(require 'init-check)
(require 'init-project)

;;;;; Prot
;; (setq find-library-include-other-files nil) ; Emacs 29
;; (setq kill-do-not-save-duplicates t)
;; (setq mode-require-final-newline 'visit-save)
;; (setq next-error-recenter '(4)) ; center of the window
;; (setq scroll-error-top-bottom t)
;; (setq trusted-content '("~/Git/Projects/")) ; Emacs 30
;; (setq-default truncate-partial-width-windows nil)

(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Init-mini.el ends here
