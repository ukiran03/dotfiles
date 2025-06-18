
;; https://github.com/abo-abo/tiny
;; Quickly generate linear ranges in Emacs
;; `example:' m5 10*xx|0x%x -> 0x19 0x24 0x31 0x40 0x51 0x64
(use-package tiny :ensure nil :disabled)


;; Interactive macro expander
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)
              :map lisp-interaction-mode-map
              ("C-c e" . macrostep-expand)))


(use-package gdb-mi
  :ensure nil
  :config
  (setq gdb-many-windows t)
  (setq gdb-show-main t))
