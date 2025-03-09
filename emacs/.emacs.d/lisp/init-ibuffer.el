;;; init-ibuffer.el --- summary -*- lexical-binding: t -*-


;;; Commentary:

;;; Code:
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold))))

;; Display icons for buffers
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  (setq nerd-icons-ibuffer-icon-size 1.0))

(use-package ibuffer-vc
  :disabled
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package ibuffer-project
  :hook (ibuffer . (lambda ()
                     "Group ibuffer's list by project."
                     (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
                     (unless (eq ibuffer-sorting-mode 'project-file-relative)
                       (ibuffer-do-sort-by-project-file-relative))))
  :init (setq ibuffer-project-use-cache t)
  :config
  (defun my-ibuffer-project-group-name (root type)
    "Return group name for project ROOT and TYPE."
    (if (and (stringp type) (> (length type) 0))
        (format "%s %s" type root)
      (format "%s" root)))
  (progn
    (advice-remove #'ibuffer-project-group-name #'my-ibuffer-project-group-name)
    (setq ibuffer-project-root-functions
          '((ibuffer-project-project-root . "Project")
            (file-remote-p . "Remote")))))

(use-package frog-jump-buffer
  :bind (("H-b a" . frog-jump-buffer)
         ("H-b 4 a" . frog-jump-buffer-other-window)))

(provide 'init-ibuffer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ibuffer.el ends here
