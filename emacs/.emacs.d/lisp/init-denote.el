;;; init-denote.el --- Better Note-taking configurations -*- lexical-binding: t -*-

;;; Commentary:
;; A clean, structured note-taking engine built upon Denote's predictable file names.

;;; Code:

;; Core Note Management Framework
(use-package denote
  :straight t
  :bind (("C-c n n" . denote)
         ("C-c n r" . denote-rename-file)
         ("C-c n l" . denote-link)
         ("C-c n b" . denote-backlinks)
         ("C-c n d" . denote-sort-dired)
         ("C-c n x" . my/denote-create-note-in-any-directory)) ; Integrated personal macro
  :init
  ;; Configure paths and format hooks early
  (setq denote-directory (expand-file-name "~/Documents/Notes"))
  :config
  ;; Target Storage Configurations
  (setq denote-file-type 'org
        denote-known-keywords '("meta" "system" "emacs" "media" "programming"
                                "essay" "english" "book" "film" "music"))

  ;; Automatically rewrite buffer headers to cleanly display titles instead of
  ;; raw text filenames
  (denote-rename-buffer-mode 1))

;; Consult Integration (Out-of-order searching and live grepping)
(use-package consult-denote
  :straight t
  :after (consult denote)
  :bind (("C-c n c f" . consult-denote-find)
         ("C-c n c r" . consult-denote-grep)))

(use-package denote-sequence
  :straight t
  :after denote
  :bind (("C-c n s n" . denote-sequence-new-child)
         ("C-c n s p" . denote-sequence-new-parent)))

(use-package denote-search
  :straight t
  :after denote
  :bind ("C-c n s s" . denote-search))

;;; Personal Custom Extensions
;;;###autoload
(defun my/denote-create-note-in-any-directory ()
  "Create a brand new Denote note file targeting an arbitrary chosen
location.  Prompts dynamically for target directory storage using
minibuffer completion hooks."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-directory
         (read-directory-name "Generate new note inside: " nil nil :must-match)))
    (call-interactively #'denote)))

(provide 'init-denote)
;;; init-denote.el ends here
