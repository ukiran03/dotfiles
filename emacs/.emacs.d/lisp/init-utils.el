;;; init-utils.el --- Utilities configuration -*- lexical-binding: t -*-

;;; Commentary:
;;  Collection of miscellaneous utility packages for Emacs.

;;; Code:

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package blackout
  :ensure t
  :config
  (pcase-dolist (`(,mode . ,tag)
                 '((lisp-interaction-mode . "λ")
                   (python-mode           . "Py")
                   (emacs-lisp-mode       . "Eλ")
                   (sh-mode               . "SH")
                   (js-jsx-mode           . "Js+JSX")
                   (js-mode               . "Js")
                   (racket-mode           . "RKT")
                   (scheme-mode           . "λSCM")))
    (blackout mode tag)))

(use-package drag-stuff
  :ensure t
  ;; :init (drag-stuff-global-mode 1)
  :config
  (drag-stuff-define-keys))

(use-package find-temp-file
  :ensure t
  :bind ("H-f t" . find-temp-file)
  :config
  (setq find-temp-file-directory
        (expand-file-name "emacs_tmps/" temporary-file-directory))
  ;; Create the directory if it's missing
  (unless (file-exists-p find-temp-file-directory)
    (make-directory find-temp-file-directory t)))

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
