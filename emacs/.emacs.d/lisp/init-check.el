;;; init-check.el --- Code Checker configurations.	-*- lexical-binding: t -*-

(use-package flymake
  :ensure nil
  :hook (elisp-mode . flymake-mode)
  :config
  (setq flymake-no-changes-timeout nil
        flymake-fringe-indicator-position 'right-fringe)
  (setq flymake-mode-line-lighter "FY"))

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
  ;; (setq dictionary-server "localhost") -- TODO: setup dictd on void
  )

(provide 'init-check)
