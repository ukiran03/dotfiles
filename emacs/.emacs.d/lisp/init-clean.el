;;; init-clean.el --- Keep .emacs.d clean -*- lexical-binding: t -*-

;; Handle Emacs Custom Settings cleanly
(use-package emacs
  :straight nil
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :config
  (when (file-exists-p custom-file)
    (load custom-file t)))

;; No-Littering Configuration
(use-package no-littering
  :straight t
  :demand t ; Force load early so other packages respect its paths
  :custom
  (auto-save-file-name-transforms
   `((,(concat "\\`" (file-name-as-directory temporary-file-directory)) "\\2" t)
     ("\\`/tmp/" "\\2" t)
     ("\\`/dev/shm/" "\\2" t)
     (".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (backup-directory-alist
   `((,(concat "\\`" (file-name-as-directory temporary-file-directory)) . nil)
     ("\\`/tmp/" . nil)
     ("\\`/dev/shm/" . nil)
     ("." . ,(no-littering-expand-var-file-name "backup/"))))
  :config
  ;; Exclude no-littering directories from recentf history
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

(provide 'init-clean)
;;; init-clean.el ends here
