;; init-org.el --- Org Mode configurations.	-*- lexical-binding: t -*-

;; Commentry
;; Org mode: Org capture, Org Agenda etc 
(use-package org
  :ensure nil
  :init
  (setq org-directory (expand-file-name "~/org")
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


(use-package org-modern
  :after org
  :hook ((org-mode . org-modern-mode))
  :config
  (with-eval-after-load 'org-modern
    (set-face-attribute
     'org-modern-symbol nil
     :family "Iosevka"
     :height 110
     :weight 'semibold))
  (setq org-modern-star 'replace)
  (setq org-modern-timestamp nil)
  (setq org-modern-table nil)
  (setq org-modern-checkbox nil)
  (setq org-modern-block-fringe nil)
  (setq org-modern-progress 12))

(provide 'init-org)

;;; init-org.el ends here.
