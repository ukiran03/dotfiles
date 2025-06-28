
(use-package js
  :ensure nil
  :hook (js-mode . (lambda () (super-save-mode -1)))
  :config
  (setq js-indent-level 2))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :config
  (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode)))

(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (web-mode . prettier-js-mode))
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "all"
                           "--bracket-spacing" "true"
                           "--tabWidth" "4"
                           "--useTabs" "false"
                           "--semi" "true"
                           "--singleQuote" "true"
                           )))

(use-package web-mode)

(use-package emmet-mode)

(use-package typescript-mode)

(provide 'init-web)
