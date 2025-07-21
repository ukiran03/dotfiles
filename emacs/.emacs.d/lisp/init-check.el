;;; init-check.el --- Code Checker configurations.	-*- lexical-binding: t -*-

;;; Commentry:
;; setting up code-checking

;;; Code:

(use-package flymake
  :ensure nil
  :hook (prog-mode . (lambda ()
                       (unless (derived-mode-p 'racket-mode)
                         (flymake-mode))))
  :config
  (setq flymake-no-changes-timeout nil
        flymake-fringe-indicator-position 'right-fringe)
  (setq flymake-mode-line-lighter "FY"))

(use-package flymake-ruff)

(use-package jinx
  :ensure t
  :blackout
  ;; :hook
  ;; ((prog-mode-hook
  ;;   conf-mode-hook
  ;;   text-mode-hook) . jinx-mode)
  :bind
  (([remap ispell-word] . jinx-correct)
   (:repeat-map jinx-repeat-map
                ("s" . jinx-next)
                ("S" . jinx-previous)))
  ;; :init
  ;; (keymap-set next-map "s" '("Misspelling" . jinx-next))
  ;; (keymap-set prev-map "s" '("Misspelling" . jinx-previous))
  )

(use-package dictionary
  :ensure nil
  :init
  ;; (setq dictionary-server "localhost") -- setup dictd on void
  (add-to-list 'display-buffer-alist
               '("^\\*Dictionary\\*" display-buffer-in-side-window
                 (side . right)
                 (window-width . 80))))

(use-package sideline-flymake
  :ensure t
  :blackout sideline-mode
  :custom-face
  (sideline-flymake-error ((t (:height 0.85 :italic t))))
  (sideline-flymake-warning ((t (:height 0.85 :italic t))))
  (sideline-flymake-success ((t (:height 0.85 :italic t))))
  :hook (flymake-mode . sideline-mode)
  :init (setq sideline-flymake-display-mode 'point
              sideline-backends-right '(sideline-flymake)))

(provide 'init-check)
;;; init-check.el ends here
