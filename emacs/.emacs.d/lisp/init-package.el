;;; init-package.el --- Initialize package configurations via straight.el. -*- lexical-binding: t -*-

;; Commentary:
;; Package management configuration utilizing straight.el and use-package.

;;; Code:

;;; Straight.el Bootstrap

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Use-Package Integration

;; Tell use-package to automatically use straight.el for all declarations
(setq straight-use-package-by-default t)

;; Configure base use-package options
(setq use-package-always-defer t
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

;; Load built-in use-package directly
(require 'use-package)

;; Ensure diminish works via straight
(use-package diminish
  :straight t)

(provide 'init-package)
;;; init-package.el ends here
