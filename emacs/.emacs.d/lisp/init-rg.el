;;; init-rg.el --- ripgrep utils -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package rg
  :straight t
  :defer t
  :commands (rg-menu rg-isearch-menu code-scrapers-golang)

  :bind (:map global-map
         ("M-s R" . rg-menu)
         ("M-s c" . code-scrapers-golang))

  :init
  ;; Safely inject the isearch keybinding into the built-in isearch system
  (with-eval-after-load 'isearch
    (define-key isearch-mode-map (kbd "M-s r") #'rg-isearch-menu))

  :config
  ;; Golang Scrapers
  (rg-define-search rg-go-import-blocks
    "Search Go files for multiline import blocks."
    :query "(?s)import \\(.*?\\)"
    :format regexp
    :files "*.go"
    :dir project
    :flags ("-U" "-z" "-o" "-P" "-g" "!vendor/"))

  (rg-define-search rg-go-interface-blocks
    "Search Go files for multiline interface type blocks."
    :query "(?s)type\\s+\\w+\\s+interface\\s*\\{.*?\\}"
    :format regexp
    :files "*.go"
    :dir project
    :flags ("-U" "-z" "-o" "-P" "-g" "!vendor/"))

  (rg-define-search rg-go-struct-blocks
    "Search Go files for multiline struct type blocks."
    :query "(?s)type\\s+\\w+\\s+struct\\s*\\{.*?\\}"
    :format regexp
    :files "*.go"
    :dir project
    :flags ("-U" "-z" "-o" "-P" "-g" "!vendor/"))

  ;; Transient menu for Golang scrapers
  (transient-define-prefix code-scrapers-golang ()
    "My suite of custom ripgrep AST scrapers."
    ["Go Scrapers"
     ("I" "Go Import Blocks" rg-go-import-blocks)
     ("i" "Go Interface Definitions" rg-go-interface-blocks)
     ("f" "Go Struct Definitions" rg-go-struct-blocks)]))

(provide 'init-rg)
;;; init-rg.el ends here
