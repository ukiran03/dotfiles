
(use-package rg
  :ensure t)

(use-package substitute
  :ensure t)
(use-package anzu
  :ensure t)

(use-package link-hint
  :bind (("C-c l o" . link-hint-open-link)
         ("C-c l c" . link-hint-copy-link)))

(provide 'init-search)
