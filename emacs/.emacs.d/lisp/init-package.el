;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; Package management configurations utilizing built-in package.el and use-package.

;;; Code:

(require 'package)

;; --- Package Archives & Priorities ---
;; Highest number gets priority (unmentioned archives default to 0)
(setq package-archives
      '(("gnu-elpa"       . "https://elpa.gnu.org/packages/")
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu"         . "https://elpa.nongnu.org/nongnu/")
        ("melpa"          . "https://melpa.org/packages/"))

      package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa"    . 2)
        ("nongnu"   . 1)))

;; --- Initialization ---
(setq package-enable-at-startup nil           ; Prevent dual-initialization cycles
      package-install-upgrade-built-in t)     ; Allow upgrading built-in packages via ELPA

(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; --- Use-Package Configuration ---
;; Pre-load adjustments (Must be set before requiring use-package)
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

;; Native evaluation check for Emacs 29+ built-in support
(unless (package-installed-p 'use-package)
  (require 'use-package))

;; --- Essential Utilities ---
(use-package diminish
  :ensure t)

(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
