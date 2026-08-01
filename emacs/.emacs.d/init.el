;;; init.el --- My Emacs Configuration.-*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:
;; This is the primary initialization file for Emacs. It sets up aggressive
;; startup optimizations, structures the load paths, and modularly loads
;; specific configuration subsets from the `lisp/` and `site-lisp/` directories.

;; (setq debug-on-error nil)

;;; Code:

(when (version< emacs-version "27.1")
  (error "This requires Emacs 27.1 and above!"))

;; ==========================================
;; ;;; STARTUP & PERFORMANCE OPTIMIZATIONS ;;;
;; ==========================================

;; Set GC threshold high for initialization phase to prevent micro-GC pauses
(setq gc-cons-threshold (* 50 1024 1024)) ; 50 MB
(setq gc-cons-percentage 0.6)

;; Don't pass case-insensitive regex to `auto-mode-alist'
(setq auto-mode-case-fold nil)

;; Defer file-name-handler-alist and reset GC thresholds after init finishes
(unless (or (daemonp) noninteractive init-file-debug)
  (let ((default-handlers file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                ;; Restore file-name handlers
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist default-handlers)))
                ;; Reset GC to normal operational defaults
                (setq gc-cons-threshold 800000
                      gc-cons-percentage 0.1)
                ;; Display total startup duration
                (message "Emacs started in %s." (emacs-init-time)))
              101)))

;; Native Compilation options (Emacs 28+)
(when (fboundp 'native-compile-async)
  (setq native-comp-deferred-compilation t
        comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))

;; ==========================================
;; ;;; LOAD PATH CONFIGURATION            ;;;
;; ==========================================

(defun update-load-path (&rest _)
  "Update `load-path` to force primary lisp directories to the front."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Recursively add subdirectories in `site-lisp` to `load-path`.
Avoid placing large files like EAF in `site-lisp` to prevent slow startup."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

;; Run path updates immediately for built-in/pre-loaded configuration
(update-load-path)
(add-subdirs-to-load-path)

;; ==========================================
;; ;;; TYPOGRAPHY & FONTS                  ;;;
;; ==========================================

(dolist (face '(default fixed-pitch variable-pitch))
  (set-face-attribute face nil
                      :family "Iosevka Medium"
                      :height 140))

;; ==========================================
;; ;;; CORE ARCHITECTURE MODULES          ;;;
;; ==========================================

;; MUST BE FIRST: Initializes straight.el and use-package integration
(require 'init-package)

(require 'init-steal)
(require 'init-funcs)
(require 'init-completions)
(require 'init-clean)
(require 'init-vertico)
(require 'init-prefix)
(require 'init-core)

;; ==========================================
;; ;;; NAVIGATION & INTERACTION           ;;;
;; ==========================================

(require 'init-dired)
(require 'init-consult)
(require 'init-filetree)
(require 'init-highlight)
(require 'init-simple)
(require 'init-search)
(require 'init-rg)
(require 'init-ibuffer)

;; ==========================================
;; ;;; ENVIRONMENT & UTILITIES            ;;;
;; ==========================================

(require 'init-ui)
(require 'init-org)
(require 'init-hugo)
(require 'init-denote)
(require 'init-treesit)
(require 'init-tab-bar)
(require 'init-window)
(require 'init-icons)
(require 'init-utils)
(require 'init-shell)
(require 'init-feeds)
(require 'init-modeline)
(require 'init-quotes)

;; ==========================================
;; ;;; LANGUAGE & PROGRAMMING DEVELOP     ;;;
;; ==========================================

(require 'init-ai)
(require 'init-snippets)
(require 'init-lsp)
(require 'init-langs)
(require 'init-prog)
(require 'init-dape)
(require 'init-python)
(require 'init-go)
(require 'init-c)
(require 'init-db)
(require 'init-web)
(require 'init-mail)
(require 'init-vc)
(require 'init-linting)
(require 'init-project)
(require 'init-devops)
;; (require 'init-kyverno)

;; ==========================================
;; ;;; BUILT-IN COMMAND UNBANNING          ;;;
;; ==========================================

(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here
