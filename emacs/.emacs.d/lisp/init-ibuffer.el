;;; init-ibuffer.el --- Buffer management configurations -*- lexical-binding: t -*-

;;; Commentary:
;; Groups and configures ibuffer list displays by functional project parameters.

;;; Code:

(use-package ibuffer
  :straight (:type built-in)
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold))))

;; Group ibuffer's list by project
(use-package ibuffer-project
  :autoload (ibuffer-project-generate-filter-groups
             ibuffer-do-sort-by-project-file-relative)
  :functions icons-displayable-p
  :hook (ibuffer . (lambda ()
                     "Group ibuffer's list by project."
                     (setq ibuffer-filter-groups
                           (ibuffer-project-generate-filter-groups))
                     (unless (eq ibuffer-sorting-mode 'project-file-relative)
                       (ibuffer-do-sort-by-project-file-relative))))
  :init (setq ibuffer-project-use-cache t)
  :config
  (with-no-warnings
    (defun my/ibuffer-project-group-name (root type)
      "Return group name for project ROOT and TYPE."
      (if (and (stringp type) (> (length type) 0))
          (format "%s %s" type root)
        (format "%s" root)))
    (if (icons-displayable-p)
        (progn
          (advice-add #'ibuffer-project-group-name
                      :override #'my/ibuffer-project-group-name)
          (setq
           ibuffer-project-root-functions
           `((ibuffer-project-project-root
              . ,(nerd-icons-octicon
                  "nf-oct-repo" :height 1.2 :face ibuffer-filter-group-name-face))
             (file-remote-p
              . ,(nerd-icons-codicon
                  "nf-cod-radio_tower"
                  :height 1.2 :face ibuffer-filter-group-name-face)))))
      (progn
        (advice-remove #'ibuffer-project-group-name #'my/ibuffer-project-group-name)
        (setq ibuffer-project-root-functions
              '((ibuffer-project-project-root . "Project")
                (file-remote-p . "Remote")))))))

;; Avy-style visual selection jump menus for fast buffer switching
(use-package frog-jump-buffer
  :straight t
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
