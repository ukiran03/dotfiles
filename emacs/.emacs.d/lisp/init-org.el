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
  (set-face-attribute 'org-modern-label nil
                      :family "Iosevka Extended"
                      :height 95
                      :weight 'medium)
  :config
  (setq org-modern-replace-stars "◉◈✸◇✳")
  (setq org-modern-star 'replace
        org-modern-checkbox nil
        org-modern-progress 10))

(provide 'init-org)

;;; init-org.el ends here.
