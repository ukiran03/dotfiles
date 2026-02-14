;;; init-clean.el --- summary -*- lexical-binding: t -*-

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

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
          ("." . ,(no-littering-expand-var-file-name "undo-tree-hist/")))))

(provide 'init-clean)
;;; init-clean.el ends here
