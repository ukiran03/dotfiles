
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

(provide 'init-snippets)
;;; init-snippets.el ends here
