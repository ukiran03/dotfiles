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
  :config
  (setq org-structure-template-alist
        '(("s" . "src")
          ("sh" . "src sh")
          ("c" . "src C")
          ("e" . "src emacs-lisp")
          ("h" . "src haskell")
          ("E" . "src emacs-lisp :results value code :lexical t")
          ("t" . "src emacs-lisp :tangle FILENAME")
          ("T" . "src emacs-lisp :tangle FILENAME :mkdirp yes")
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


;;  `Org-Agenda'
(use-package org
  :ensure nil
  :bind ("C-c a" . org-agenda)
  :config
  (setq calendar-week-start-day 0)
  (setq org-agenda-files '("~/Documents/org/tasks.org")) ; -- Original
  ;; (setq org-agenda-files '("~/Documents/org/demo.org")) ; -- Demo
  ;; (setq org-agenda-files (list org-directory))
  )

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
           (file+olp "tasks.org" "All Tasks" "Wishlist")
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

(provide 'init-org)

;;; init-org.el ends here.
