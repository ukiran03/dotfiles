;;; init-clean.el --- summary -*- lexical-binding: t -*-

;;; Keep .emacs.d clean

(use-package no-littering
  :ensure t
  :demand
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)

  (setq auto-save-file-name-transforms
        `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
           ,(concat (file-name-as-directory temporary-file-directory) "\\2") t)
          ("\\`/tmp\\([^/]*/\\)*\\(.*\\)\\'" "\\2")
          ("\\`/dev/shm\\([^/]*/\\)*\\(.*\\)\\'" "\\2")
          (".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-directory-alist
        `((,(concat "\\`" (file-name-as-directory temporary-file-directory)))
          ("\\`/tmp/" . nil)
          ("\\`/dev/shm/" . nil)
          ("." . ,(no-littering-expand-var-file-name "backup/"))))
  (setq undo-tree-history-directory-alist
        `((,(concat "\\`" (file-name-as-directory temporary-file-directory)))
          ("\\`/tmp/" . nil)
          ("\\`/dev/shm/" . nil)
          ("." . ,(no-littering-expand-var-file-name "undo-tree-hist/"))))
  )

;; (setq backup-directory-alist
;;       `(("." . "~/.emacs.d/data/backups"))
;;       auto-save-file-name-transforms
;;       `((".*" "~/.emacs.d/data/auto-saves/" t))
;;       auto-save-list-file-prefix
;;       (expand-file-name "data/auto-saves/sessions" user-emacs-directory)
;;       )

(provide 'init-clean)
;;; init-clean.el ends here
