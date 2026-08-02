;;; init-project.el --- Project management configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Configures the built-in project.el library with enhanced keybindings,
;; custom root markers, a cleaner mode-line, and a custom file viewer.

;;; Code:

(use-package project
  :straight nil
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
  (add-to-list 'project-vc-ignores "vendor/")
  (setq project-vc-extra-root-markers '(".project" "project.org" "PROJECT"))  ; Emacs 29

  ;; Modern Emacs 28/29+ switch command format using character literals
  (setq project-switch-commands
        '((project-view-file "View File" ?F)
          (project-find-file "Find file" ?f)
          (project-dired "Dired" ?\r)
          (project-find-dir "Find directory" ?d)
          (rg-project "Ripgrep" ?r)
          (project-eshell "Eshell" ?e)
          (magit-project-status "Magit" ?g)
          (project-any-command "Other" ?o)))

  (defun project-view-file (&optional arg)
    "Open a file inside the current project using `view-mode'.
With prefix ARG (\\[universal-argument]), open in another window."
    (interactive "P")
    (let* ((pr (project-current t))
           (dirs (project-roots pr))
           (files (project-files pr dirs))
           (file (project-find-file-in (thing-at-point 'filename) files pr)))
      (if arg
          (view-file-other-window file)
        (view-file file)))))

(provide 'init-project)
;;; init-project.el ends here
