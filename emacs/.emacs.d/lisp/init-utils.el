;;; init-utils.el --- Utilities configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Collection of miscellaneous utility packages for Emacs.

;;; Code:

(use-package emamux
  :straight t)

(use-package trashed
  :disabled t
  :straight t
  :commands (trashed)
  :custom
  (trashed-action-confirmer 'y-or-n-p)
  (trashed-use-header-line t)
  (trashed-sort-key '("Date deleted" . t))
  (trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package drag-stuff
  :straight t
  ;; :init (drag-stuff-global-mode 1)
  :config
  (drag-stuff-define-keys))

(use-package find-temp-file
  :straight t
  :custom
  (find-temp-file-directory
   (expand-file-name "emacs_tmps/" temporary-file-directory))
  :bind
  ("H-f t" . find-temp-file)
  :config
  ;; Create the directory if it's missing
  (unless (file-exists-p find-temp-file-directory)
    (make-directory find-temp-file-directory t)))

(use-package caps-lock
  :straight t)

(provide 'init-utils)
;;; init-utils.el ends here
