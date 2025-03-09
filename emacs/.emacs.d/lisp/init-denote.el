;; init-denote.el --- Better Note-tatking configurations.	-*- lexical-binding: t -*-

;;; Commentry:
;; Best Note taking software

(use-package consult-denote
  :bind ("C-c n f" . consult-denote-find))

(use-package denote
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-sort-dired))
  :config
  (setq denote-directory (expand-file-name "~/Documents/Notes"))
  (setq denote-known-keywords '("meta" "system" "emacs" "media"
			                    "programming" "essay" "english" "book"  "film" "music"))
  (denote-rename-buffer-mode 1))

(provide 'init-denote)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-denote.el ends here
