;; init-treemacs.el --- Initialize treemacs.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Treemacs: A tree layout file explorer.
;;

;;; Code:

(eval-when-compile
  (require 'init-funcs))

(use-package projtree
  :load-path ("site-lisp/emacs-projtree/")
  :ensure nil
  :commands (projtree-mode))


(provide 'init-filetree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treemacs.el ends here
