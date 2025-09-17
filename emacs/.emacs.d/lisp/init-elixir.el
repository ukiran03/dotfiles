;; Elixir setup with tree-sitter
;; (use-package elixir-ts-mode
;;   :ensure nil ;; built into Emacs 29+
;;   :mode (("\\.ex\\'"  . elixir-ts-mode)
;;          ("\\.exs\\'" . elixir-ts-mode))
;;   :hook ((elixir-ts-mode . apheleia-mode)
;;          (elixir-ts-mode . symbol-overlay-mode))
;;   :custom
;;   (elixir-ts-mode-indent-offset 2))

(use-package elixir-mode
  :ensure t
  :hook ((elixir-mode . apheleia-mode)
         (elixir-mode . symbol-overlay-mode)))

;; HEEx templates (Phoenix LiveView)
(use-package heex-ts-mode
  :ensure nil ;; built into Emacs 29+
  :mode ("\\.heex\\'" . heex-ts-mode)
  :hook ((heex-ts-mode . apheleia-mode)
         (heex-ts-mode . symbol-overlay-mode)
         (heex-ts-mode . emmet-mode)) ;; check `web-mode'
  :custom
  (heex-ts-mode-indent-offset 2))

(provide 'init-elixir)
