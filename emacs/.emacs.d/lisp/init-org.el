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

  :bind
  (:map org-mode-map
        ("M-." . org-edit-special)
        :map org-src-mode-map
        ("M-," . org-edit-src-exit))
  :config
  (setq org-structure-template-alist
        '(("s" . "src")
          ("b" . "src bash")
          ("q" . "src sql")
          ("g" . "src go")
          ("t" . "src text")
          ("e" . "src emacs-lisp")
          ("v" . "src verb")
          ("eE" . "src emacs-lisp :results value code :lexical t")
          ("et" . "src emacs-lisp :tangle FILENAME")
          ("eT" . "src emacs-lisp :tangle FILENAME :mkdirp yes")
          ("x" . "example")
          ("X" . "export")
          ;; ("q" . "quote")
          ))
  (setq org-modules nil
        org-startup-indented nil
        org-ellipsis (if (char-displayable-p ?⏷) "\t⏷" nil)
        org-pretty-entities nil
        org-hide-emphasis-markers t))

(use-package org
  :ensure nil
  :hook ((org-mode . org-indent-mode))
  :config
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-cycle-separator-lines 0)
  (setq org-hide-leading-stars nil)
  (setq org-hide-macro-markers nil)
  (setq org-insert-heading-respect-content t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-use-fast-todo-selection 'expert)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w)" "|" "CANCELLED(c@)" "MISSED(m@/!)" "DONE(d!)")))
  (setq org-indent-mode-turns-on-hiding-stars t)
  (setq org-adapt-indentation nil)
  (setq org-indent-indentation-per-level 2) ; 4
  (setq org-startup-folded 'content))

(use-package org-modern
  :diminish
  :after org
  :hook (
         ;; (org-mode                 . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda)
         (org-mode                 . org-indent-mode))
  :config
  (setq org-modern-star 'replace
        ;; org-modern-replace-stars "◉◈✸◇✳"
        org-modern-replace-stars "✳"
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


;;;;;  Org-Agenda

(defvar uk-org-custom-daily-agenda
  ;; Match everything and then skip entries with `org-agenda-skip-function'.
  `((tags-todo "*"
               ((org-agenda-overriding-header "Important tasks without a date")
                (org-agenda-skip-function #'uk-org-agenda-include-priority-no-timestamp)
                (org-agenda-block-separator nil)))
    (agenda "" ((org-agenda-overriding-header "\nPending scheduled tasks")
                (org-agenda-start-on-weekday nil)
                (org-agenda-span 1)
                (org-agenda-show-all-dates nil)
                (org-scheduled-past-days 365)
                (org-scheduled-delay-days 1)
                (org-agenda-block-separator nil)
                (org-agenda-entry-types '(:scheduled))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-format-date "")))
    (agenda "" ((org-agenda-overriding-header "\nToday's agenda")
                (org-agenda-span 1)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-scheduled-past-days 0)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "ROUTINE"))
                (org-agenda-format-date "%A %-e %B %Y")))
    (agenda "" ((org-agenda-overriding-header "\nNext three days")
                (org-agenda-start-on-weekday nil)
                (org-agenda-start-day nil)
                (org-agenda-start-day "+1d")
                (org-agenda-span 3)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))
    (agenda "" ((org-agenda-overriding-header "\nUpcoming deadlines (+14d)")
                (org-agenda-start-on-weekday nil)
                ;; We don't want to replicate the previous section's
                ;; three days, so we start counting from the day after.
                (org-agenda-start-day "+4d")
                (org-agenda-span 14)
                (org-agenda-show-all-dates nil)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-entry-types '(:deadline))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done)))))
  "Custom agenda for use in `org-agenda-custom-commands'.")

(use-package org
  :ensure nil
  :bind (("C-c A" . org-agenda)
         ("C-c a" . (lambda () (interactive) (org-agenda nil "A"))))
  :config
  (setq org-agenda-files (list org-directory))
  (setq org-agenda-remove-times-when-in-prefix nil)
  (setq org-agenda-sorting-strategy
        '(((agenda habit-down time-up priority-down category-keep)
           (todo priority-down category-keep)
           (tags priority-down category-keep)
           (search category-keep))))
  (setq org-agenda-follow-indirect t)
  (setq org-scheduled-past-days 365)
  (setq org-deadline-past-days 365)
  (setq org-agenda-time-leading-zero t)
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-format-date #'prot-org-agenda-format-date-aligned)
  (setq org-agenda-custom-commands
        `(("A" "Daily agenda and top priority tasks"
           ,uk-org-custom-daily-agenda
           ((org-agenda-fontify-priorities nil)
            (org-agenda-prefix-format "	 %t %s")
            (org-agenda-dim-blocked-tasks nil)))
          ("P" "Plain text daily agenda and top priorities"
           ,uk-org-custom-daily-agenda
           ((org-agenda-with-colors nil)
            (org-agenda-prefix-format "%t %s")
            (org-agenda-fontify-priorities nil)
            (org-agenda-remove-tags t))
           ("agenda.txt"))))
  :config
  ;; Prioritised tasks
  (defun uk-org-agenda-include-priority-no-timestamp ()
    "Return nil if heading has a priority but no timestamp.
Otherwise, return the buffer position from where the search should
continue, per `org-agenda-skip-function'."
    (let ((point (point)))
      (if (and (eq (nth 3 (org-heading-components)) ?A)
               (not (org-get-deadline-time point))
               (not (org-get-scheduled-time point)))
          nil (line-beginning-position 2))))
  ;; custom time format
  (defun prot-org-agenda-format-date-aligned (date)
    "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading.
Slightly tweaked version of `org-agenda-format-date-aligned' that
produces dates with a fixed length."
    (require 'cal-iso)
    (let* ((dayname (calendar-day-name date t))
           (day (cadr date))
           (day-of-week (calendar-day-of-week date))
           (month (car date))
           (monthname (calendar-month-name month t))
           (year (nth 2 date))
           (iso-week (org-days-to-iso-week
                      (calendar-absolute-from-gregorian date)))
           (weekstring (if (= day-of-week 1) (format " (W%02d)" iso-week) "")))
      (format "%s %2d %s %4d%s" dayname day monthname year weekstring))))

;;;; `Org-Capture'
(use-package org
  :ensure nil
  :bind ("C-c c" . org-capture)
  :config
  (setq org-capture-templates
        (let* ((without-time
                (concat ":PROPERTIES:\n"
                        ":CAPTURED: %U\n"
                        ":CUSTOM_ID: h:%(format-time-string \"%Y%m%dT%H%M%S\")\n"
                        ":END:\n\n"))
               (with-time
                (concat "DEADLINE: %^T\n"
                        ":PROPERTIES:\n"
                        ":CAPTURED: %U\n"
                        ":CUSTOM_ID: h:%(format-time-string \"%Y%m%dT%H%M%S\")\n"
                        ":APPT_WARNTIME: 20\n"
                        ":END:\n\n")))
          `(("t" "Tasks to do")
            ("tt" "Work tasks" entry    ; Primary
             (file+headline "tasks.org" "Work")
             ,(concat "* TODO %^{Title} \n" without-time "%a\n%?")
             :empty-lines-after 1)
            ("th" "Home tasks" entry
             (file+headline "tasks.org" "Home")
             ,(concat "* TODO %^{Title} \n" without-time "%?")
             :empty-lines-after 1)
            ("tp" "Personal tasks" entry
             (file+headline "tasks.org" "Personal")
             ,(concat "* TODO %^{Title} \n" without-time "%?")
             :empty-lines-after 1)
            ("w" "Wishlist" entry
             (file+headline "tasks.org" "Wishlist")
             ,(concat "* %^{Title} \n" without-time "%a\n%?")
             :empty-lines-after 1)
            ("u" "Unprocessed" entry
             (file+headline "tasks.org" "Unprocessed")
             ,(concat "* %^{Title}\n" without-time "%?"))
            ;; ("i" "Capture Ideas")
            ("p" "Project Ideas" entry
             (file+headline "ideas.org" "Projects")
             ,(concat "* %^{Title} \n" without-time "%?")
             :empty-lines-after 1)))))

(use-package org
  :ensure nil
  :config
  (setq org-archive-location
        (concat (file-name-as-directory (expand-file-name "archive" org-directory))
                "%s_archive::")))


;;;; Hugo

(use-package ox-hugo
  :ensure t
  :after org
  :config
  ;; Populates only the EXPORT_FILE_NAME property in the inserted heading.
  (with-eval-after-load 'org-capture
    (defun org-hugo-new-subtree-post-capture-template ()
      "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
      (let* ((title (read-from-minibuffer "Post Title: "))
             (fname (org-hugo-slug title)))
        (mapconcat #'identity
                   `(,(concat "* TODO " title)
                     ":PROPERTIES:" ,(concat ":EXPORT_FILE_NAME: " fname)
                     ":END:" "%?\n") "\n")))

    (add-to-list 'org-capture-templates
                 '("h"                ;`org-capture' binding + h
                   "Hugo post" entry
                   ;; It is assumed that below file is present in `org-directory'
                   ;; and that it has a "Blog Ideas" heading. It can even be a
                   ;; symlink pointing to the actual location of all-posts.org!
                   (file+olp "~/.tmp/hugo/quickstart/content-org/all-posts.org"
                             "Test Post")
                   (function org-hugo-new-subtree-post-capture-template)))))

;;; Babel
;;;; code blocks

(use-package org
  :ensure nil
  :bind (:map org-mode-map
              ("C-c j" . org-babel-goto-named-src-block))
  :config
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)
  (setq org-edit-src-persistent-message nil)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)

  (defconst load-language-alist
    '((emacs-lisp . t)
      (python     . t)
      (C          . t)
      (shell      . t))
    "Alist of org ob languages.")
  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-alist))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-alist))


(provide 'init-org)

;;; init-org.el ends here.
