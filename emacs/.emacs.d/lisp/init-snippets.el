;;; init-snippets.el --- Snippets configurations. -*- lexical-binding: t -*-

;;; Commentary:
;; Setting up in-buffer snippets and templates via Yasnippet and Tempel.

;;; Code:

;; Yasnippet Configuration
(use-package yasnippet
  :straight t
  :hook
  (prog-mode . yas-minor-mode))

;; Collection of ready-made yasnippet snippets
(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

;; Integrate Yasnippet with Consult for easy searching
(use-package consult-yasnippet
  :straight t
  :bind
  ("M-g y" . consult-yasnippet))

;; Integrate Yasnippet into Completion At Point (Corfu/Company)
(use-package yasnippet-capf
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;; Tempel Configuration (Modern, Fast Alternative)
(use-package tempel
  :straight t
  :preface
  (defun tempel-setup-capf ()
    ;; Add the Tempel capf to the list.
    ;; `add-hook' allows combining it with other backends.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :bind
  (("M-+" . tempel-complete) ;; Alternative tempel-expand
   ("M-*" . tempel-insert))
  :hook
  ((conf-mode . tempel-setup-capf)
   (prog-mode . tempel-setup-capf)
   (text-mode . tempel-setup-capf)))

;; Global & Mode-Specific Abbrevs
(use-package abbrev
  :straight (:type built-in)
  :custom
  (abbrev-mode t)
  :config
  (define-abbrev global-abbrev-table "mygit" "https://github.com/ukiran03")
  (define-abbrev text-mode-abbrev-table
    "mylocation"
    ""))

(provide 'init-snippets)
;;; init-snippets.el ends here
