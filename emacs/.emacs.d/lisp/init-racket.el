;;; init-racket.el --- summary -*- lexical-binding: t -*-


;;; Commentary:

;;; Code:


;; SICP: <https://stackoverflow.com/questions/74169756/sicp-evaluate-racket-into-repl-with-emacs-and-geiser>
;; (use-package geiser
;;   :init
;;   ;; (setq geiser-active-implementations '(guile))
;;   (setq geiser-scheme-implementation 'guile)
;;   :config
;;   (use-package geiser-guile))


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
  ;; (set-face-attribute 'racket-xp-unused-face nil :strike-through nil)
  (custom-set-faces
   '(racket-repl-prompt ((t (:inherit font-lock-function-name-face :weight bold)))))
  ;; (setq racket-repl-buffer-name-function
  ;;       #'racket-repl-buffer-name-project) TODO:
  )

;; TODO:
;; (use-package consult
;;   :ensure nil
;;   :config
;;   (with-eval-after-load 'consult
;;     (add-to-list 'consult-mode-histories 'racket-repl-mode)))

(use-package racket-fmt-format
  :load-path ("site-lisp/racket-fmt-format")
  :config
  (add-to-list 'auto-mode-alist '("\\.rkt\\'" . (lambda () (racket-mode) (racket-fmt-format-on-save-mode)))))


(provide 'init-racket)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-racket.el ends here
