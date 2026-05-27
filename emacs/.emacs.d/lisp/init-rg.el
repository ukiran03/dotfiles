;;; init-rg.el --- ripgrep utils -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package deadgrep
  :disabled
  :bind ("M-s C-r" . deadgrep))

(use-package rg
  :ensure t
  :bind ((:map isearch-mode-map
               ("M-s r" . rg-isearch-menu))
         (:map global-map
               ("M-s R" . rg-menu)
               ("M-s c" . code-scrapers-golang)))
  :config
  ;; Wrap macro definitions inside :config so they load safely after 'rg' is
  ;; loaded.

  ;;;; Golang Scrapers
  (rg-define-search rg-go-import-blocks
    "Search Go files for multiline import blocks."
    :query "(?s)import \\(.*?\\)" ; (?s) activates dotall mode (dot matches newlines)
    :format regexp
    :files "*.go"
    :dir project
    :flags ("-U" "-z" "-o" "-P"))

  (rg-define-search rg-go-interface-blocks
    "Search Go files for multiline interface type blocks."
    :query "(?s)type\\s+\\w+\\s+interface\\s*\\{.*?\\}"
    :format regexp
    :files "*.go"
    :dir project
    :flags ("-U" "-z" "-o" "-P"))

  (rg-define-search rg-go-struct-blocks
    "Search Go files for multiline struct type blocks."
    :query "(?s)type\\s+\\w+\\s+struct\\s*\\{.*?\\}"
    :format regexp
    :files "*.go"
    :dir project
    :flags ("-U" "-z" "-o" "-P"))

  ;; Transient menu for Golang scrapers Note: `transient` is built-in to modern
  ;; Emacs, but depends on `rg` macros being defined first.
  (transient-define-prefix code-scrapers-golang ()
    "My suite of custom ripgrep AST scrapers."
    ["Go Scrapers"
     ("I" "Go Import Blocks" rg-go-import-blocks)
     ("i" "Go Interface Definitions" rg-go-interface-blocks)
     ("f" "Go Struct Definitions" rg-go-struct-blocks)]))

(provide 'init-rg)
;;; init-rg.el ends here
