;;; init-vertico.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; `Library:' vertico-directory, vertico-unobtrusive
;; vertico-autoloads.el vertico-buffer.el vertico-directory.el
;; vertico-flat.el vertico-grid.el vertico-indexed.el vertico-mouse.el
;; vertico-multiform.el vertico-quick.el vertico-repeat.el
;; vertico-reverse.el vertico-sort.el vertico-suspend.el
;; vertico-unobtrusive.el vertico.el

(use-package vertico
  :ensure t
  :after minibuffer
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :bind (:map vertico-map
              ("M-q" . 'vertico-quick-exit)
              ("C-q" . 'vertico-quick-insert)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :config
  (setq vertico-cycle t
        vertico-scroll-margin 2
        vertico-group-format nil
        vertico-resize t)
  (use-package vertico-multiform
    :ensure nil
    :after vertico
    :init (vertico-multiform-mode 1)
    :config
    (setq vertico-multiform-categories
          '((jinx grid (vertico-grid-annotate . 20))
            (t reverse)))
    (setq vertico-multiform-commands
          '((consult-grep buffer)
            (consult-git-grep buffer)
            (consult-ripgrep buffer)
            (consult-locate buffer)
            (consult-find buffer)
            (consult-fd buffer)
            (consult-outline buffer indexed)
            (consult-imenu buffer indexed)))))

;; M-B -> `vertico-multiform-buffer'
;; M-F -> `vertico-multiform-flat'
;; M-G -> `vertico-multiform-grid'
;; M-R -> `vertico-multiform-reverse'
;; M-U -> `vertico-multiform-unobtrusive'
;; M-V -> `vertico-multiform-vertical'

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vertico.el ends here
(provide 'init-vertico)
