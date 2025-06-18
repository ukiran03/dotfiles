;;; init-snippets.el --- Snippets configurations.	-*- lexical-binding: t -*-

;;; Commentry:
;; Setting-up in buffer snippets

;; Code:
;; ‘Snippets’ Settings'

;; Yet another snippet extension
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode))   ; instead of `yas-global-mode'

;; Collection of yasnippet snippets
(use-package yasnippet-snippets)


(use-package consult-yasnippet
  :bind ("M-g y" . consult-yasnippet))

;; Yasnippet Completion At Point Function
(use-package yasnippet-capf
  :init (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;; Configure Tempel
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection)

;; Optional: Use the Corfu completion UI
;; (use-package corfu
;;   :init
;;   (global-corfu-mode))


(provide 'init-snippets)
;;; init-snippets.el ends here
