;; init-org.el --- Org Mode configurations.	-*- lexical-binding: t -*-

;; Commentry
;; Org mode: Org capture, Org Agenda etc
(use-package org
  :ensure nil
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

  ;; Learn about the ! and more by reading the relevant section of the
  ;; Org manual.  Evaluate: (info "(org) Tracking TODO state changes")
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)"))))

;; (setq-default org-display-custom-times t)
;; (setq org-time-stamp-custom-formats '("<%a %b %e %Y>" . "<%a %b %e %Y %H:%M>"))

;;  `Org-Agenda'
(use-package org
  :ensure nil
  :config
  (setq org-agenda-files (list org-directory)))

(use-package remember
  :ensure nil
  :hook (remember-mode . page-break-lines-mode)
  :config
  (setq remember-notes-buffer-name "*remember*")
  (setq remember-notes-initial-major-mode 'text-mode)
  ;; (setq remember-leader-text "\f\n** ")
  (setq remember-data-file (expand-file-name "remember" org-directory)))

(use-package org-modern
  :after org
  :hook ((org-mode                 . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda))
  :init
  (global-org-modern-mode)
  ;; (set-face-attribute 'org-modern-label nil
  ;;                     ;; :family "Iosevka Extended"
  ;;                     :height 95
  ;;                     ;; :weight 'medium
  ;;                     )
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
        org-modern-radio-target nil
        ))

(use-package org-super-agenda
  :ensure t
  :after org
  :hook ((org-agenda-mode . org-super-agenda-mode))
  :config
  (let ((org-super-agenda-groups
         '(;; Each group has an implicit boolean OR operator between its selectors.
           (:name "Today"  ; Optionally specify section name
                  :time-grid t  ; Items that appear on the time grid
                  :todo "TODAY")  ; Items that have this TODO keyword
           (:name "Important"
                  ;; Single arguments given alone
                  :tag "bills"
                  :priority "A")
           ;; Set order of multiple groups at once
           (:order-multi (2 (:name "Shopping in town"
                                   ;; Boolean AND group matches items that match all subgroups
                                   :and (:tag "shopping" :tag "@town"))
                            (:name "Food-related"
                                   ;; Multiple args given in list with implicit OR
                                   :tag ("food" "dinner"))
                            (:name "Personal"
                                   :habit t
                                   :tag "personal")
                            (:name "Space-related (non-moon-or-planet-related)"
                                   ;; Regexps match case-insensitively on the entire entry
                                   :and (:regexp ("space" "NASA")
                                                 ;; Boolean NOT also has implicit OR between selectors
                                                 :not (:regexp "moon" :tag "planet")))))
           ;; Groups supply their own section names when none are given
           (:todo "WAITING" :order 8)  ; Set order of this section
           (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                  ;; Show this group at the end of the agenda (since it has the
                  ;; highest number). If you specified this group last, items
                  ;; with these todo keywords that e.g. have priority A would be
                  ;; displayed in that group instead, because items are grouped
                  ;; out in the order the groups are listed.
                  :order 9)
           (:priority<= "B"
                        ;; Show this section after "Today" and "Important", because
                        ;; their order is unspecified, defaulting to 0. Sections
                        ;; are displayed lowest-number-first.
                        :order 1)
           ;; After the last group, the agenda will display items that didn't
           ;; match any of these groups, with the default order position of 99
           )))
    (org-agenda nil "a")))

(provide 'init-org)

;;; init-org.el ends here.
