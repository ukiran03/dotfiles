;;; init-snippets.el --- Snippets configurations.	-*- lexical-binding: t -*-

;;; Commentry:
;; Setting-up in buffer snippets

;; Code:
;; ‘Snippets’ Settings'

;; Yet another snippet extension
(use-package yasnippet
  ;; :disabled
  :blackout yas-minor-mode
  ;; :hook (prog-mode . yas-minor-mode)
  )

;; Collection of yasnippet snippets
(use-package yasnippet-snippets)


(use-package consult-yasnippet
  :disabled
  :bind ("M-g y" . consult-yasnippet))

;; Yasnippet Completion At Point Function
(use-package yasnippet-capf
  :disabled
  :init (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;; Configure Tempel
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom (tempel-trigger-prefix "<")
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)))

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
;; (use-package tempel-collection)

;;;;; using compile-hook compile-command
;; (use-package tempo
;;   :ensure nil
;;   :config
;;   (tempo-define-template "go-compile"
;;                          `("// -*- compile-command: \"go run "
;;                            (file-name-nondirectory buffer-file-name) "\" -*-" n n)
;;                          nil
;;                          "Insert Go compile-command header"))

;; (use-package auto-insert
;;   :ensure nil
;;   :init
;;   (auto-insert-mode 1)
;;   :config
;;   (define-auto-insert 'go-mode #'tempo-template-go-compile))

(define-abbrev global-abbrev-table "mygit" "https://github.com/ukiran03")
(define-abbrev text-mode-abbrev-table
  "mylocation"
  "Sri Sai Balaji CO-Live PG, # 98, 14TH C Cross, Neeladri Nagar, Younis
khan Layout, Electronic City, Bengaluru, Karnataka 560100")



(provide 'init-snippets)
;;; init-snippets.el ends here
