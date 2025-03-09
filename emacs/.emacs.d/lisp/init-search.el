;;; init-search.el --- summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;;; Isearch, occur, grep, and extras

;; (use-package isearch
;;   :ensure nil
;;   :demand t
;;   :config
;;   (setq search-whitespace-regexp ".*?" ; one `setq' here to make it obvious they are a bundle
;;         isearch-lax-whitespace t
;;         isearch-regexp-lax-whitespace nil))

(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq search-highlight t)
  (setq isearch-lazy-highlight t)
  (setq lazy-highlight-initial-delay 0.5)
  (setq lazy-highlight-no-delay-length 4))

(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil))

(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq isearch-wrap-pause t) ; `no-ding' makes keyboard macros never quit
  (setq isearch-repeat-on-direction-change t))

(use-package rg
  :ensure t
  :bind ((:map isearch-mode-map
               ("M-s r" . rg-isearch-menu))
         (:map global-map
               ("M-s R" . rg-menu))))
(use-package deadgrep
  :bind ("M-s C-r" . deadgrep))

(use-package substitute
  :ensure t)

(use-package link-hint
  :bind (("C-c l o" . link-hint-open-link)
         ("C-c l c" . link-hint-copy-link)))

(provide 'init-search)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-search.el ends here
