;;; init-utils.el --- Utilities configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Collection of miscellaneous utility packages for Emacs.

;;; Code:

(use-package trashed
  :disabled
  :straight t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package drag-stuff
  :straight t
  ;; :init (drag-stuff-global-mode 1)
  :config
  (drag-stuff-define-keys))

(use-package find-temp-file
  :straight t
  :bind ("H-f t" . find-temp-file)
  :config
  (setq find-temp-file-directory
        (expand-file-name "emacs_tmps/" temporary-file-directory))
  ;; Create the directory if it's missing
  (unless (file-exists-p find-temp-file-directory)
    (make-directory find-temp-file-directory t)))

(use-package caps-lock
  :straight t)

(provide 'init-utils)
;;; init-utils.el ends here
