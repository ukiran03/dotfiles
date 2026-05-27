;;; init-ibuffer.el --- Buffer management configurations -*- lexical-binding: t -*-

;;; Commentary:
;; Groups and configures ibuffer list displays by functional project parameters.

;;; Code:

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold))))

(use-package ibuffer-project
  :ensure t
  ;; Fix: Replaced the long inline lambda with a clean, named function hook
  :hook (ibuffer . my-ibuffer-project-group-and-sort)
  :init
  (setq ibuffer-project-use-cache t)
  :config
  (defun my-ibuffer-project-group-and-sort ()
    "Group ibuffer's list dynamically by current active project and sort contextually."
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative)))

  (defun my-ibuffer-project-group-name (root type)
    "Format function to override name generation formatting for project ROOT and TYPE."
    (if (and (stringp type) (> (length type) 0))
        (format "%s %s" type root)
      (format "%s" root)))

  ;; Apply the override advice so your custom naming schema takes effect
  (advice-add #'ibuffer-project-group-name :override #'my-ibuffer-project-group-name)

  (defun my-ibuffer-remote-root (buf)
    "Safely return the remote connection string descriptor for BUF if applicable."
    (with-current-buffer buf
      (when (and default-directory (file-remote-p default-directory))
        (file-remote-p default-directory))))

  ;; Wire up project and remote filters safely
  (setq ibuffer-project-root-functions
        '((ibuffer-project-project-root . "Project")
          (my-ibuffer-remote-root       . "Remote"))))

;; Avy-style visual selection jump menus for fast buffer switching
(use-package frog-jump-buffer
  :ensure t
  :bind (("H-b a"   . frog-jump-buffer)
         ("H-b 4 a" . frog-jump-buffer-other-window)))

;; Interactive Buffer generation primitive utilities
(defun new-empty-buffer ()
  "Create a new scratch-style empty temporary buffer.
New buffer will be named “untitled”, “untitled<2>”, etc.
It returns the generated buffer structure tracking item pointer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    buf))

(provide 'init-ibuffer)

;;; init-ibuffer.el ends here
