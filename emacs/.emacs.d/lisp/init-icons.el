;;; init-icons.el --- Icon integrations using nerd-icons -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:
;; Configures nerd-icons and extensions for completion prompts, corfu,
;; dired, ibuffer, xref, and grep.

;;; Code:

;; Core Nerd Icons Package
(use-package nerd-icons
  :straight t)

(use-package nerd-icons-completion
  :straight t
  :if (display-graphic-p)
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

  ;; By default, icons are shown in all sorts of completion prompts.
  ;; When those have different kinds of candidates, like files and
  ;; folders, the icons are helpful.  If all the candidates have the
  ;; same icon though, I prefer not to see any icon.
  (setq nerd-icons-completion-category-icons nil))

(use-package nerd-icons-corfu
  :straight t
  :if (display-graphic-p)
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :disabled
  :straight t
  :if (display-graphic-p)
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :straight t
  :if (display-graphic-p)
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
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

(use-package nerd-icons-xref
  :straight t
  :if (display-graphic-p)
  :after xref
  :config
  (nerd-icons-xref-mode 1))

(use-package nerd-icons-grep
  :straight t
  :if (display-graphic-p)
  :after grep
  :config
  (when grep-use-headings
    (nerd-icons-grep-mode 1)))

(provide 'init-icons)
;;; init-icons.el ends here
