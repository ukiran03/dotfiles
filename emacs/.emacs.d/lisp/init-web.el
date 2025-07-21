
;; JS/JSX Setup
(use-package js
  :ensure nil
  :mode (("\\.js\\'" . js-jsx-mode))
  :hook ((js-jsx-mode . apheleia-mode)
         (js-jsx-mode . emmet-mode)
         (js-jsx-mode . (lambda () (super-save-mode -1))))
  :config
  (setq js-indent-level 2)
  (add-to-list 'interpreter-mode-alist '("node" . js-jsx-mode)))

;; Emmet for JSX/HTML expansion
(use-package emmet-mode
  :hook (js-jsx-mode . emmet-mode)
  :blackout (emmet-mode . " </>")
  :config
  (setq emmet-expand-jsx-className? t))

(use-package prettier-js
  :blackout (prettier-js-mode . " ;")
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "all"
                           "--bracket-spacing" "true"
                           "--tabWidth" "4"
                           "--useTabs" "false"
                           "--semi" "true"
                           "--singleQuote" "true"
                           )))

;; JS/JSX Setup
(use-package typescript-mode
  :ensure nil
  :mode (("\\.ts\\'" . tsx-ts-mode))
  :hook ((tsx-ts-mode . apheleia-mode)
         (tsx-ts-mode . emmet-mode)
         (tsx-ts-mode . (lambda () (super-save-mode -1))))
  :config
  (add-to-list 'interpreter-mode-alist '("node" . tsx-ts-mode)))


;;;;; Not Yet/Working

(use-package web-mode
  :disabled
  :mode (("\\.jsx?\\'" . web-mode))
  :hook (web-mode . emmet-mode)
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        set-web-mode-content-type "jsx"))

(use-package js2-mode
  :disabled
  :mode (("\\.js\\'" . js2-mode))
  :config
  (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode)))
(use-package typescript-mode)

(provide 'init-web)
