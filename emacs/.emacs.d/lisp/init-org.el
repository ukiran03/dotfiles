;; init-org.el --- Org Mode configurations.	-*- lexical-binding: t -*-

;; Commentry
;; Org mode: Org capture, Org Agenda etc

(use-package org
  :ensure nil
  :bind (:map org-mode-map
              ("H-n b" . org-narrow-to-block)
              ("H-n e" . org-narrow-to-element)
              ("H-n s" . org-narrow-to-subtree))
  :init
  (setq org-directory (expand-file-name "~/Documents/org")
        org-imenu-depth 7)
  (add-to-list 'safe-local-variable-values '(org-hide-leading-stars . t))
  (add-to-list 'safe-local-variable-values '(org-hide-macro-markers . t))
  :bind
  (:map org-mode-map
        ("M-." . org-edit-special)
        :map org-src-mode-map
        ("M-," . org-edit-src-exit))
  :config
  (setq org-structure-template-alist
        '(("s" . "src")
          ("b" . "src bash")
          ("g" . "src go")
          ("t" . "src text")
          ("e" . "src emacs-lisp")
          ("v" . "src verb")
          ("eE" . "src emacs-lisp :results value code :lexical t")
          ("et" . "src emacs-lisp :tangle FILENAME")
          ("eT" . "src emacs-lisp :tangle FILENAME :mkdirp yes")
          ("x" . "example")
          ("X" . "export")
          ("q" . "quote")))
  (setq org-modules nil
        org-startup-indented t
        org-ellipsis (if (char-displayable-p ?⏷) "\t⏷" nil)
        org-pretty-entities nil
        org-hide-emphasis-markers t))

(use-package org
  :ensure nil ; do not try to install it as it is built-in
  :config
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-insert-heading-respect-content t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)"))))

(use-package org-modern
  :after org
  :hook ((org-mode                 . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda))
  :config
  (setq org-modern-star 'replace
        org-modern-replace-stars "◉◈✸◇✳"
        org-modern-checkbox nil
        org-modern-tag nil
        org-modern-progress nil
        org-modern-label-border nil
        org-modern-timestamp nil
        org-modern-table nil
        org-modern-priority nil
        ;; org-modern-list nil
        org-modern-horizontal-rule t
        org-modern-todo nil
        org-modern-block-name nil
        org-modern-keyword nil
        org-modern-internal-target nil
        org-modern-radio-target nil))

(use-package org-appear)

;;;; code blocks
(use-package org
  :ensure nil
  :config
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)
  (setq org-edit-src-persistent-message nil)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0))


;;  `Org-Agenda'
(use-package org
  :ensure nil
  :bind ("C-c a" . org-agenda)
  :config
  (setq calendar-week-start-day 0)
  (setq org-agenda-files '("~/Documents/org/tasks.org")))

;; `Org-Capture'
(use-package org
  :ensure nil
  :bind ("C-c c" . org-capture)
  :config
  (setq org-capture-templates
        `(("u" "Unprocessed" entry
           (file+headline "tasks.org" "Unprocessed")
           ,(concat "* %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":CUSTOM_ID: h:%(format-time-string \"%Y%m%dT%H%M%S\")\n"
                    ":END:\n\n"
                    "%a\n%i%?")
           :empty-lines-after 1)
          ("w" "Wishlist" entry
           (file+headline "tasks.org" "Wishlist")
           ,(concat "* %^{Title} %^g\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":CUSTOM_ID: h:%(format-time-string \"%Y%m%dT%H%M%S\")\n"
                    ":END:\n\n"
                    "%a\n%?")
           :empty-lines-after 1)
          ("t" "Task to do" entry
           (file+headline "tasks.org" "All Tasks")
           ,(concat "* TODO %^{Title} %^g\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":CUSTOM_ID: h:%(format-time-string \"%Y%m%dT%H%M%S\")\n"
                    ":END:\n\n"
                    "%a\n%?")
           :empty-lines-after 1))))

(use-package ox-hugo
  :ensure t
  :after org
  :config
  ;; Populates only the EXPORT_FILE_NAME property in the inserted heading.
  (with-eval-after-load 'org-capture
    (defun org-hugo-new-subtree-post-capture-template ()
      "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
      (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
             (fname (org-hugo-slug title)))
        (mapconcat #'identity
                   `(
                     ,(concat "* TODO " title)
                     ":PROPERTIES:"
                     ,(concat ":EXPORT_FILE_NAME: " fname)
                     ":END:"
                     "%?\n")          ;Place the cursor here finally
                   "\n")))

    (add-to-list 'org-capture-templates
                 '("h"                ;`org-capture' binding + h
                   "Hugo post"
                   entry
                   ;; It is assumed that below file is present in `org-directory'
                   ;; and that it has a "Blog Ideas" heading. It can even be a
                   ;; symlink pointing to the actual location of all-posts.org!
                   (file+olp "~/.tmp/hugo/quickstart/content-org/all-posts.org" "Test Post")
                   (function org-hugo-new-subtree-post-capture-template)))))


(provide 'init-org)

;;; init-org.el ends here.
