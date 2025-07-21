;;; init-icon.el --- summary -*- lexical-binding: t no-byte-compile: t -*-

;; (use-package nerd-icons-completion
;;   :hook (after-init-hook . nerd-icons-completion-mode)
;;   :init
;;   (setq nerd-icons-completion-icon-size 0.8)
;;   :config
;;   (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-completion
  :when (icons-displayable-p)
  :hook (vertico-mode . nerd-icons-completion-mode))
;; :config
;; (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))


(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode-hook . nerd-icons-ibuffer-mode)
  :init
  (setq nerd-icons-ibuffer-icon-size 0.8)
  (setq nerd-icons-ibuffer-formats
        '((mark " " (icon 2 2)
                " " (name 20 20 :left :elide)
                " " modified read-only locked
                " " (size-h 7 -1 :right)
                " " (mode+ 16 16 :left :elide)
                " " filename-and-process+)
          (mark " " name))))

(use-package nerd-icons-dired
  :blackout
  :when (icons-displayable-p)
  :custom-face
  (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'init-icon)
;;; init-icon.el ends here)
