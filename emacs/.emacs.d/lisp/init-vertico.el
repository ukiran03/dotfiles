;;; init-vertico.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; `find-library' => vertico, vertico-quick, vertico-multiform, vertico-directory, vertico-unobtrusive

(use-package vertico
  :ensure t
  :after minibuffer
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :bind (:map vertico-map
              ("C-;" . vertico-quick-exit)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :config
  (setq vertico-cycle t
        vertico-scroll-margin 2 ;; Different scroll margin 0 - 2
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
            (consult-imenu buffer indexed)
            (consult-buffer flat)))))

;; M-B -> `vertico-multiform-buffer'
;; M-F -> `vertico-multiform-flat'
;; M-G -> `vertico-multiform-grid'
;; M-R -> `vertico-multiform-reverse'
;; M-U -> `vertico-multiform-unobtrusive'
;; M-V -> `vertico-multiform-vertical'

;; ("<left>" . backward-char)
;; ("<right>" . forward-char)
;; ("C-'" . vertico-quick-exit)
;; ("?"   . minibuffer-completion-help)

;; <https://github.com/minad/vertico/blob/main/README.org#completion-at-point-and-completion-in-region>
;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
;; (setq completion-in-region-function
;;       (lambda (&rest args)
;;         (apply (if vertico-mode
;;                    #'consult-completion-in-region
;;                  #'completion--in-region)
;;                args)))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vertico.el ends here
(provide 'init-vertico)
