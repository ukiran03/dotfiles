;; init-denote.el --- Better Note-tatking configurations.	-*- lexical-binding: t -*-

;;; Commentry:
;; Best Note taking software

(use-package consult-denote
  :bind (("C-c n c f" . consult-denote-find)
         ("C-c n c r" . consult-denote-grep)))

(use-package denote
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-sort-dired))
  :config
  (setq denote-file-type 'org)
  (setq denote-directory (expand-file-name "~/Documents/Notes"))
  (setq denote-known-keywords '("meta" "system" "emacs" "media"
			                    "programming" "essay" "english" "book"  "film" "music"))
  (denote-rename-buffer-mode 1))

(use-package denote-org)
(use-package denote-journal)
(use-package denote-sequence)
(use-package denote-search)

(defun my-denote-create-note-in-any-directory ()
  "Create new Denote note in any directory.
     Prompt for the directory using minibuffer completion."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-directory (read-directory-name "New note in: " nil nil :must-match)))
    (call-interactively #'denote)))

;; denote packages: denote, denote-journal, denote-sequence,
;; denote-org, denote-markdown, denote-silo, denote-menu, citar-denote

(provide 'init-denote)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-denote.el ends here
