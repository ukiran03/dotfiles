;; `find-library' => vertico, vertico-quick, vertico-multiform, vertico-directory, vertico-unobtrusive

(use-package vertico
  :ensure t
  :after minibuffer
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :config
  (setq vertico-cycle t
        vertico-scroll-margin 2 ;; Different scroll margin 0 - 2
        ;; vertico-count 20     ;; Show more candidates
        vertico-resize t)
  (use-package vertico-multiform          ;; M-B -> `vertico-multiform-buffer'
    :ensure nil                           ;; M-F -> `vertico-multiform-flat'
    :after vertico                        ;; M-G -> `vertico-multiform-grid'
    :init (vertico-multiform-mode 1)      ;; M-R -> `vertico-multiform-reverse'
    :config                               ;; M-U -> `vertico-multiform-unobtrusive'
    (setq vertico-multiform-commands      ;; M-V -> `vertico-multiform-vertical'
          '((dired grid)
            (consult-recent-file grid)
            (consult-grep buffer)
            (consult-git-grep buffer)
            (consult-theme grid)
            (consult-imenu buffer indexed)
            (consult-ripgrep buffer)))))

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

(provide 'init-vertico)
