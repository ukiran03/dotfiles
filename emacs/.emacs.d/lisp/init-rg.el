;;; init-rg.el --- ripgrep utils -*- lexical-binding: t -*-

(use-package deadgrep
  :bind ("M-s C-r" . deadgrep))
(use-package rg
  :ensure t
  :bind ((:map isearch-mode-map
               ("M-s r" . rg-isearch-menu))
         (:map global-map
               ("M-s R" . rg-menu))))

;;;; Golang
(rg-define-search rg-go-import-blocks
  "Search Go files for multiline import blocks."
  :query "import \\([\\s\\S]*?\\)"
  :format regexp
  :files "*.go"
  :dir project
  :flags ("-U" "-z" "-o"))

(rg-define-search rg-go-interface-blocks
  "Search Go files for multiline interface type blocks."
  :query "type\\s+\\w+\\s+interface\\s*{[\\s\\S]*?}"
  :format regexp
  :files "*.go"
  :dir project
  :flags ("-U" "-z" "-o" "-P"))

(rg-define-search rg-go-struct-blocks
  "Search Go files for multiline struct type blocks."
  :query "type\\s+\\w+\\s+struct\\s*{[\\s\\S]*?}"
  :format regexp
  :files "*.go"
  :dir project
  :flags ("-U" "-z" "-o" "-P"))

(provide 'init-rg)
