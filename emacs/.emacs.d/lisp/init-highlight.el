;;; init-highlight.el --- Initialize highlighting configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; Highlighting configurations.

;;; Code:

;; Highlight the current line safely
(use-package hl-line
  :ensure nil
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode eat-mode vterm-mode) .
          (lambda () (hl-line-mode -1))))) ; Fix: Explicitly disable locally

(use-package lin :ensure nil :defer t)

;; Smooth visual line/region pulsing
(use-package pulsar
  :ensure t
  :defer t
  :commands (pulsar-pulse-line pulsar-highlight-dwim)
  :config
  (setq pulsar-pulse t
        pulsar-delay 0.03
        pulsar-iterations 10
        pulsar-face 'pulsar-generic
        pulsar-highlight-face 'pulsar-cyan
        pulsar-pulse-on-window-change t)

  ;; Efficient, clean list additions
  (setq pulsar-pulse-region-functions
        (append '(easy-kill yank kill-rectangle uk-kill-whole-line
                            transpose-words transpose-chars transpose-lines
                            yank-rectangle open-rectangle undo primitive-undo)
                pulsar-pulse-region-functions))

  (setq pulsar-pulse-functions
        (append '(ding avy-goto-char-2 avy-goto-line avy-goto-word-1
                       avy-goto-char-timer avy-copy-line avy-kill-whole-line
                       pop-to-mark-command pop-global-mark goto-last-change
                       exit-minibuffer abort-minibuffers)
                pulsar-pulse-functions))
  :hook
  (;; Handles both startup graphics and Emacs daemon server frame allocations
   (server-after-make-frame . (lambda ()
                                (when (display-graphic-p)
                                  (pulsar-global-mode 1))))
   (next-error . pulsar-pulse-line)
   (imenu-after-jump . pulsar-recenter-center)
   (imenu-after-jump . pulsar-reveal-entry)
   (consult-after-jump . pulsar-recenter-center)
   (consult-after-jump . pulsar-reveal-entry))
  :bind
  (("C-c p p" . pulsar-pulse-line)
   ("C-c p h" . pulsar-highlight-dwim)))

;; Highlight matching parens
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Syntax highlighting of known Elisp symbols
(use-package highlight-defined
  :ensure t
  :hook ((emacs-lisp-mode inferior-emacs-lisp-mode) . highlight-defined-mode))

;; Efficient cursor jump reference highlights
(use-package symbol-overlay
  :ensure t
  :blackout t
  :init
  (setq symbol-overlay-idle-time 0.1)
  :bind (:map symbol-overlay-mode-map
              ("M-i"  . symbol-overlay-put)
              ("M-n"  . symbol-overlay-jump-next)
              ("M-p"  . symbol-overlay-jump-prev)
              ("M-N"  . symbol-overlay-switch-forward)
              ("M-P"  . symbol-overlay-switch-backward)
              ("M-C"  . symbol-overlay-remove-all)
              ([M-f3] . symbol-overlay-remove-all)))

;; In-buffer hex and literal color visualizer
(use-package colorful-mode
  :ensure t
  :blackout t
  :init
  (setq colorful-use-prefix nil)
  ;; (setq colorful-prefix-string "⬤")
  :config
  (dolist (mode '(html-mode php-mode help-mode helpful-mode))
    (add-to-list 'global-colorful-modes mode)))

;; Keyword / Tag highlighting and project searches
(use-package hl-todo
  :ensure t
  :bind (:map hl-todo-mode-map
              ("C-c t o" . hl-todo-occur)
              ("C-c t p" . hl-todo-previous)
              ("C-c t n" . hl-todo-next)
              ("C-c t i" . hl-todo-insert)
              ("C-c t r" . hl-todo-rg-project)
              ("C-c t R" . hl-todo-rg))
  :hook ((after-init . global-hl-todo-mode)
         (hl-todo-mode . (lambda ()
                           (add-hook 'flymake-diagnostic-functions
                                     #'hl-todo-flymake nil t))))
  :init
  (setq hl-todo-require-punctuation t
        hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"   error bold)
          ("NOTE"   warning bold)
          ("REDO"   warning bold)
          ("Q"      warning bold)
          ("A"      font-lock-constant-face bold)
          ("REVIEW" font-lock-constant-face bold)
          ("FIXME"  error bold)))
  :config
  ;; Silence compiler warning for rip-grep interface integration functions
  (declare-function rg "ext:rg")
  (declare-function rg-project "ext:rg")
  (declare-function rg-read-files "ext:rg")

  (defun hl-todo-rg (regexp &optional files dir)
    "Use `rg' to find all TODO: or similar keywords."
    (interactive
     (progn
       (unless (require 'rg nil t)
         (user-error "`rg' package is not installed"))
       (let ((regexp (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp))))
         (list regexp
               (rg-read-files)
               (read-directory-name "Base directory: " nil default-directory t)))))
    (rg regexp files dir))

  (defun hl-todo-rg-project ()
    "Use `rg' to find all TODO or similar keywords in current project."
    (interactive)
    (unless (require 'rg nil t)
      (user-error "`rg' package is not installed"))
    (rg-project
     (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp)) "everything")))

(provide 'init-highlight)

;;; init-highlight.el ends here
