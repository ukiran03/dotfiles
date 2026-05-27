;;; init-org.el --- Org Mode configurations.	-*- lexical-binding: t -*-

;; Commentary: Comprehensive Org mode setup, featuring Agenda, Capture, Babel,
;; and Hugo integrations.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 1. Helper Functions & Custom Variables (Defined early to prevent void errors)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun uk-org-agenda-include-priority-no-timestamp ()
  "Return nil if heading has a priority but no timestamp.
Otherwise, return the buffer position from where the search should continue."
  (let ((point (point)))
    (if (and (eq (nth 3 (org-heading-components)) ?A)
             (not (org-get-deadline-time point))
             (not (org-get-scheduled-time point)))
        nil (line-beginning-position 2))))

(defun prot-org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda.
Produces fixed-length, perfectly aligned dates."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date t))
         (day (cadr date))
         (day-of-week (calendar-day-of-week date))
         (month (car date))
         (monthname (calendar-month-name month t))
         (year (nth 2 date))
         (iso-week (org-days-to-iso-week (calendar-absolute-from-gregorian date)))
         (weekstring (if (= day-of-week 1) (format " (W%02d)" iso-week) "")))
    (format "%s %2d %s %4d%s" dayname day monthname year weekstring)))

(defvar uk-org-custom-daily-agenda
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
                (org-agenda-start-day "+1d")
                (org-agenda-span 3)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))
    (agenda "" ((org-agenda-overriding-header "\nUpcoming deadlines (+14d)")
                (org-agenda-start-on-weekday nil)
                (org-agenda-start-day "+4d")
                (org-agenda-span 14)
                (org-agenda-show-all-dates nil)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-entry-types '(:deadline))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done)))))
  "Custom agenda blocks for use in `org-agenda-custom-commands'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2. Primary Org Mode Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :ensure nil
  :hook ((org-mode . org-indent-mode))
  :bind (("C-c A" . org-agenda)
         ("C-c a" . (lambda () (interactive) (org-agenda nil "A")))
         ("C-c c" . org-capture)
         :map org-mode-map
         ("H-n b" . org-narrow-to-block)
         ("H-n e" . org-narrow-to-element)
         ("H-n s" . org-narrow-to-subtree)
         ("M-."   . org-edit-special)
         ("C-c j" . org-babel-goto-named-src-block)
         :map org-src-mode-map
         ("M-,"   . org-edit-src-exit))

  :init
  (setq org-directory (expand-file-name "~/Documents/org")
        org-imenu-depth 7)

  :config
  ;; --- Visuals & Interface ---
  (setq org-modules nil
        org-startup-indented nil
        org-ellipsis (if (char-displayable-p ?⏷) "\t⏷" nil)
        org-pretty-entities nil
        org-hide-emphasis-markers t
        org-hide-leading-stars nil
        org-hide-macro-markers nil
        org-startup-folded 'content)

  ;; --- Behavior & Structure Editing ---
  (setq org-M-RET-may-split-line '((default . nil))
        org-cycle-separator-lines 0
        org-insert-heading-respect-content t
        org-adapt-indentation nil
        org-indent-indentation-per-level 2
        org-indent-mode-turns-on-hiding-stars t)

  ;; --- Todo Settings & Logging ---
  (setq org-log-done 'time
        org-log-into-drawer t
        org-use-fast-todo-selection 'expert
        org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "CANCELLED(c@)" "MISSED(m@/!)" "DONE(d!)")))

  ;; --- Structure Templates ---
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
          ("X" . "export")))

  ;; --- Org Agenda Setup ---
  (setq org-agenda-files (list org-directory)
        org-agenda-remove-times-when-in-prefix nil
        org-agenda-follow-indirect t
        org-scheduled-past-days 365
        org-deadline-past-days 365
        org-agenda-time-leading-zero t
        org-agenda-window-setup 'current-window
        org-agenda-format-date #'prot-org-agenda-format-date-aligned)

  (setq org-agenda-sorting-strategy
        '(((agenda habit-down time-up priority-down category-keep)
           (todo priority-down category-keep)
           (tags priority-down category-keep)
           (search category-keep))))

  (setq org-agenda-custom-commands
        `(("A" "Daily agenda and top priority tasks"
           ,uk-org-custom-daily-agenda
           ((org-agenda-fontify-priorities nil)
            (org-agenda-prefix-format "\t %t %s")
            (org-agenda-dim-blocked-tasks nil)))
          ("P" "Plain text daily agenda and top priorities"
           ,uk-org-custom-daily-agenda
           ((org-agenda-with-colors nil)
            (org-agenda-prefix-format "%t %s")
            (org-agenda-fontify-priorities nil)
            (org-agenda-remove-tags t))
           ("agenda.txt"))))

  ;; --- Org Capture Setup ---
  (setq org-archive-location (concat (file-name-as-directory (expand-file-name "archive" org-directory)) "%s_archive::"))

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
            ("tt" "Work tasks" entry (file+headline "tasks.org" "Work")
             ,(concat "* TODO %^{Title} \n" without-time "%a\n%?") :empty-lines-after 1)
            ("th" "Home tasks" entry (file+headline "tasks.org" "Home")
             ,(concat "* TODO %^{Title} \n" without-time "%?") :empty-lines-after 1)
            ("tp" "Personal tasks" entry (file+headline "tasks.org" "Personal")
             ,(concat "* TODO %^{Title} \n" without-time "%?") :empty-lines-after 1)
            ("w" "Wishlist" entry (file+headline "tasks.org" "Wishlist")
             ,(concat "* %^{Title} \n" without-time "%a\n%?") :empty-lines-after 1)
            ("u" "Unprocessed" entry (file+headline "tasks.org" "Unprocessed")
             ,(concat "* %^{Title}\n" without-time "%?"))
            ("p" "Project Ideas" entry (file+headline "ideas.org" "Projects")
             ,(concat "* %^{Title} \n" without-time "%?") :empty-lines-after 1))))

  ;; --- Org Babel Configuration ---
  (setq org-confirm-babel-evaluate nil
        org-src-window-setup 'current-window
        org-edit-src-persistent-message nil
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0)

  (defconst load-language-alist
    '((emacs-lisp . t)
      (python     . t)
      (C          . t)
      (shell      . t))
    "Alist of org ob languages.")

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-alist))

  (org-babel-do-load-languages 'org-babel-load-languages load-language-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 3. Ecosystem & Third-Party Extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-modern
  :ensure t
  :after org
  :hook ((org-agenda-finalize . org-modern-agenda))
  :config
  (setq org-modern-star 'replace
        org-modern-replace-stars "✳"
        org-modern-checkbox nil
        org-modern-tag nil
        org-modern-progress nil
        org-modern-label-border nil
        org-modern-timestamp nil
        org-modern-table nil
        org-modern-priority nil
        org-modern-horizontal-rule t
        org-modern-todo nil
        org-modern-block-name nil
        org-modern-keyword nil
        org-modern-internal-target nil
        org-modern-radio-target nil))

(use-package org-appear
  :ensure t
  :after org
  :hook (org-mode . org-appear-mode))

(use-package ox-hugo
  :ensure t
  :after org
  :config
  (with-eval-after-load 'org-capture
    (defun org-hugo-new-subtree-post-capture-template ()
      "Returns `org-capture' template string for new Hugo post."
      (let* ((title (read-from-minibuffer "Post Title: "))
             (fname (org-hugo-slug title)))
        (mapconcat #'identity
                   `(,(concat "* TODO " title)
                     ":PROPERTIES:" ,(concat ":EXPORT_FILE_NAME: " fname)
                     ":END:" "%?\n") "\n")))

    (add-to-list 'org-capture-templates
                 '("h" "Hugo post" entry
                   (file+olp "~/.tmp/hugo/quickstart/content-org/all-posts.org" "Test Post")
                   (function org-hugo-new-subtree-post-capture-template)))))

(provide 'init-org)

;;; init-org.el ends here.
