;;; init-project.el --- summary -*- lexical-binding: t -*-


;;; Commentary:

;;; Code:


(use-package project
  :ensure nil
  :init
  (setq project-mode-line-face '(:inherit font-lock-constant-face :weight bold))
  :bind
  (("C-x p ." . project-dired)
   ("C-x p C-g" . keyboard-quit)
   ("C-x p <return>" . project-dired)
   ("C-x p <delete>" . project-forget-project))
  :config
  (setq project-mode-line t)
  (advice-add 'project-mode-line-format :filter-return
              (lambda (s) (when s (concat s " "))))
  (setq project-vc-extra-root-markers '(".project"))  ; Emacs 29
  (add-to-list 'project-switch-commands
               '(project-view-file "View File" "F"))

  (defun project-view-file (&optional arg)
    "Open a file in view-mode using project-find-file.
TODO: With prefix ARG (\\[universal-argument]), on `other-window'."
    (interactive "P")
    (call-interactively 'project-find-file)
    (view-mode 1)))

(provide 'init-project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-project.el ends here
