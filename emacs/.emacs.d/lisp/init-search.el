;;; init-search.el --- Search configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Isearch, occur, grep, and search extras.

;;; Code:

(use-package isearch
  :straight (:type built-in)
  :demand t
  :custom
  ;; Whitespace & Regex Behavior
  (search-whitespace-regexp ".*?")
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace nil)

  ;; Lazy Highlighting
  (search-highlight t)
  (isearch-lazy-highlight t)
  (lazy-highlight-initial-delay 0.5)
  (lazy-highlight-no-delay-length 4)

  ;; Match Count Display (e.g., (1/10))
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil)

  ;; Wrapping & Navigation
  (isearch-wrap-pause t) ; `no-ding' makes keyboard macros never quit
  (isearch-repeat-on-direction-change t))

;;; Extras & External Packages

(use-package ctrlf
  :disabled t
  :straight t
  :custom
  (ctrlf-auto-recenter t)
  (ctrlf-highlight-current-line nil)
  (ctrlf-show-match-count-at-eol nil)
  :hook
  (after-init . ctrlf-mode))

(use-package substitute
  :straight t)

(use-package link-hint
  :straight t ; Added explicitly for consistency with third-party packages
  :bind
  (("C-c l o" . link-hint-open-link)
   ("C-c l c" . link-hint-copy-link)
   ("C-c l O" . link-hint-open-link-at-point)
   ("C-c l C" . link-hint-copy-link-at-point)))

;; Affe (Asynchronous Fuzzy Finder)
(use-package affe
  :straight t
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key "M-."))

(provide 'init-search)
;;; init-search.el ends here
