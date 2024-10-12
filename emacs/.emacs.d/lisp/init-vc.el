
(use-package magit :ensure t)
(use-package forge :ensure nil)

(use-package diff-hl)

(use-package magit
  :ensure t
  ;; :bind ("C-c g" . magit-status)
  :init
  (setq magit-define-global-key-bindings nil)
  ;; (setq magit-section-visibility-indicator '("⮧"))
  :config
  (setq git-commit-summary-max-length 50)
  ;; NOTE 2023-01-24: I used to also include `overlong-summary-line'
  ;; in this list, but I realised I do not need it.  My summaries are
  ;; always in check.  When I exceed the limit, it is for a good
  ;; reason.
  (setq git-commit-style-convention-checks '(non-empty-second-line))

  (setq magit-diff-refine-hunk t))


;; Highlight uncommitted changes using VC
(use-package diff-hl
  :custom (diff-hl-draw-borders nil)
;;   :custom-face
;;   (diff-hl-change ((t (:inherit custom-changed :foreground unspecified :background unspecified))))
;;   (diff-hl-insert ((t (:inherit diff-added :background unspecified))))
;;   (diff-hl-delete ((t (:inherit diff-removed :background unspecified))))
;;   :bind (:map diff-hl-command-map
;;          ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (after-init . global-diff-hl-show-hunk-mouse-mode)
         (dired-mode . diff-hl-dired-mode))
;;   :config
;;   ;; Highlight on-the-fly
;;   (diff-hl-flydiff-mode 1)

;;   ;; Set fringe style
  (setq-default fringes-outside-margins t))


(provide 'init-vc)
