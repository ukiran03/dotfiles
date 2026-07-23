;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-

;;; Code:

(require 'package)

;; --- Package Archives & Priorities ---
(setq package-archives
      '(("gnu"           . "https://elpa.gnu.org/packages/")
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu"         . "https://elpa.nongnu.org/nongnu/")
        ("melpa"          . "https://melpa.org/packages/"))

      package-archive-priorities
      '(("gnu"    . 3)
        ("melpa"  . 2)
        ("nongnu" . 1)))

;; --- Initialization ---
(setq package-enable-at-startup nil
      package-install-upgrade-built-in t)

(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; Refresh package list if archives are empty
(unless package-archive-contents
  (package-refresh-contents))

;; --- Use-Package Configuration ---
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

(require 'use-package)

;; FORCE-EVALUATE autoloads after any auto-installation
(advice-add 'package-install :after
            (lambda (&rest _)
              (package-activate-all)))

(use-package diminish
  :ensure t)

(provide 'init-package)
;;; init-package.el ends here
