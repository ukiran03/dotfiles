
(use-package js
  :ensure nil
  :mode (("\\.js\\'" . js-jsx-mode))
  :hook ((js-mode . prettier-js-mode)
         ;; (js-mode . (lambda () (super-save-mode -1)))
         )
  :config
  (setq js-indent-level 2)
  (add-to-list 'interpreter-mode-alist '("node" . js-jsx-mode)))

(use-package js2-mode
  :disabled
  :mode (("\\.js\\'" . js2-mode))
  :config
  (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode)))

(use-package prettier-js
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
