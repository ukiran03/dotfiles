;;; init-clean.el --- Keep .emacs.d clean -*- lexical-binding: t -*-

;; 1. Handle Emacs Custom Settings cleanly
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file t)) ; Crucial step: actually load the file!

;; 2. No-Littering Configuration
(use-package no-littering
  :ensure t
  :demand t ; Force load early so other packages respect its paths
  :config
  ;; Exclude no-littering directories from recentf history
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))

  ;; Note: no-littering automatically handles standard backups and auto-saves.
  ;; We only keep your custom rules here if you absolutely need the /tmp and /dev/shm exclusions:
  (setq auto-save-file-name-transforms
        `((,(concat "\\`" (file-name-as-directory temporary-file-directory)) "\\2" t)
          ("\\`/tmp/" "\\2" t)
          ("\\`/dev/shm/" "\\2" t)
          (".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  (setq backup-directory-alist
        `((,(concat "\\`" (file-name-as-directory temporary-file-directory)) . nil)
          ("\\`/tmp/" . nil)
          ("\\`/dev/shm/" . nil)
          ("." . ,(no-littering-expand-var-file-name "backup/")))))

(provide 'init-clean)
;;; init-clean.el ends here
