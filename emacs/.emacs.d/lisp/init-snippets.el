;;; init-snippets.el --- Snippets configurations. -*- lexical-binding: t -*-

;;; Commentary:
;; Setting up in-buffer snippets and templates via Yasnippet and Tempel.

;;; Code:

;; Yasnippet Configuration
(use-package yasnippet
  :blackout yas-minor-mode
  :hook (prog-mode . yas-minor-mode))

;; Collection of ready-made yasnippet snippets
(use-package yasnippet-snippets
  :after yasnippet)

;; Integrate Yasnippet with Consult for easy searching
(use-package consult-yasnippet
  :bind ("M-g y" . consult-yasnippet))

;; Integrate Yasnippet into Completion At Point (Corfu/Company)
(use-package yasnippet-capf
  :init
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;; Tempel Configuration (Modern, Fast Alternative)
(use-package tempel
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))
  :init
  ;; Function to setup Tempel hooks for completion-at-point
  (defun tempel-setup-capf ()
    ;; Add the Tempel capf to the list.
    ;; `add-hook' allows combining it with other backends.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :hook ((conf-mode . tempel-setup-capf)
         (prog-mode . tempel-setup-capf)
         (text-mode . tempel-setup-capf)))

;; Global & Mode-Specific Abbrevs
(setq-default abbrev-mode t)

(define-abbrev global-abbrev-table "mygit" "https://github.com/ukiran03")

(define-abbrev text-mode-abbrev-table
  "mylocation"
  "Sri Sai Balaji CO-Live PG, # 98, 14TH C Cross, Neeladri Nagar, Younis khan Layout, Electronic City, Bengaluru, Karnataka 560100")

(provide 'init-snippets)
;;; init-snippets.el ends here
