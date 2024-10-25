;; SICP: <https://stackoverflow.com/questions/74169756/sicp-evaluate-racket-into-repl-with-emacs-and-geiser>
(use-package geiser
  :ensure t
  :config
  ;; Set up Geiser for Racket
  (setq geiser-active-implementations '(racket))
  (setq geiser-mode-start-repl-p t))

;; (setq geiser-scheme-implementation '(guile))
(use-package geiser
  :init
  ;; (setq geiser-active-implementations '(guile))
  (setq geiser-scheme-implementation 'guile)
  :config
  (use-package geiser-guile))

;; There is also `lsp' support for racket-lang
;; NOTE: <https://github.com/jeapostrophe/racket-langserver>
(use-package racket-mode
  :ensure t
  :hook (racket-mode . racket-xp-mode)
  :bind (:map racket-mode-map
              ("C-c C-c" . racket-run)
              ("C-c C-r" . racket-repl))
  :config
  (use-package eldoc
    :ensure nil
    :bind (:map racket-mode-map
                ("C-h ." . eldoc-print-current-symbol-info))
    :config
    (setq eldoc-echo-area-use-multiline-p nil))
  :config
  (setq racket-xp-eldoc-level 'complete) ;'minimal, 'summary
  (setq racket-show-functions '(racket-show-echo-area)))

(use-package racket-mode
  :config
  (custom-set-faces
   '(racket-repl-prompt ((t (:inherit font-lock-function-name-face))))))

(provide 'init-racket)
