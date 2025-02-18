;; init-highlight.el --- Initialize highlighting configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Highlighting configurations.
;;

;;; Code:

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))

(use-package lin :ensure nil)                       ;by `Prot'

(use-package pulsar
  :ensure t
  :init
  (pulsar-global-mode 1)
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.05)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-generic)
  (setq pulsar-highlight-face 'pulsar-cyan)
  (setq pulsar-pulse-on-window-change t)
  ;; (advice-add #'kill-ring-save :before #'pulsar-pulse-region)
  ;; (add-to-list 'kill-ring-save 'pulsar-pulse-functions)
  (dolist (func '(
                  avy-goto-char-2
                  avy-goto-line
                  avy-goto-word-1
                  avy-goto-char-timer
                  avy-copy-line
                  avy-kill-whole-line
                  uk-toggle-window-split
                  aw-select
                  ace-window
                  keyboard-quit
                  popper-toggle
                  pop-to-mark-command
                  pop-global-mark
                  goto-last-change
                  kill-ring-save        ; DEBUG
                  exit-minibuffer
                  abort-minibuffers))
    (add-to-list 'pulsar-pulse-functions func))
  :hook
  ((next-error . pulsar-pulse-line)
   (minibuffer-setup . pulsar-pulse-line-red)
   (imenu-after-jump . pulsar-recenter-center)
   (imenu-after-jump . pulsar-reveal-entry)
   (consult-after-jump . pulsar-recenter-center)
   (consult-after-jump . pulsar-reveal-entry))
  :bind
  (("C-c p p" . pulsar-pulse-line)   ; override `count-lines-page'
   ("C-c p h" . pulsar-highlight-dwim)) ; or use `pulsar-highlight-line'
  )


(use-package pulse
  ;; :disabled                             ; using pulsar-pulse-region
  :ensure nil
  :config
  (defun re-uk-pulse-current-region (&rest _)
    "Pulse the current implicit or active region."
    (if (region-active-p)
        (pulse-momentary-highlight-region (region-beginning) (region-end))
      (when (mark)
        (pulse-momentary-highlight-region (mark) (point)))))
  (advice-add #'kill-ring-save :before #'re-uk-pulse-current-region))


;; Pulse modified region
(use-package goggles
  :diminish
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)
  (setq-default goggles-pulse-delay 0.05)) ;; set to nil to disable pulsing

;; Highlight matching parens
(use-package paren
  :ensure nil
  :config (show-paren-mode 1)
  (setq show-paren-when-point-inside-paren t
	    show-paren-when-point-in-periphery t))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Syntax highlighting of known Elisp symbols
(use-package highlight-defined
  :hook ((emacs-lisp-mode inferior-emacs-lisp-mode) . highlight-defined-mode))

;; highlight-numbers
(use-package highlight-numbers
  :disabled
  :hook ((prog-mode text-mode) . highlight-numbers-mode))

(use-package symbol-overlay
  :ensure t
  ;; :disabled
  :diminish
  :bind (("M-i" . symbol-overlay-put)
	     ("M-n" . symbol-overlay-jump-next)
	     ("M-p" . symbol-overlay-jump-prev)
	     ("M-N" . symbol-overlay-switch-forward)
	     ("M-P" . symbol-overlay-switch-backward)
	     ("M-C" . symbol-overlay-remove-all)
	     ([M-f3] . symbol-overlay-remove-all))
  :hook (((prog-mode yaml-mode) . symbol-overlay-mode)
	     (iedit-mode            . turn-off-symbol-overlay)
         ;; (racket-mode           . turn-off-symbol-overlay)
	     (iedit-mode-end        . turn-on-symbol-overlay))
  :init (setq symbol-overlay-idle-time 0.1)
  :config
  (with-no-warnings
    ;; Disable symbol highlighting while selecting
    (defun turn-off-symbol-overlay (&rest _)
      "Turn off symbol highlighting."
      (interactive)
      (symbol-overlay-mode -1))
    (advice-add #'set-mark :after #'turn-off-symbol-overlay)
    (defun turn-on-symbol-overlay (&rest _)
      "Turn on symbol highlighting."
      (interactive)
      (when (derived-mode-p 'prog-mode 'yaml-mode)
	    (symbol-overlay-mode 1)))
    (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay)))

(use-package colorful-mode
  :diminish
  :hook (prog-mode . global-colorful-mode)
  :init (setq colorful-use-prefix t
              colorful-prefix-string "⬤")
  :config (dolist (mode '(html-mode php-mode help-mode helpful-mode))
            (add-to-list 'global-colorful-modes mode)))

;; <https://github.com/jdtsmith/indent-bars>
(use-package indent-bars
  :ensure t
  :disabled
  :hook ((python-mode yaml-mode) . indent-bars-mode)
  :config
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.2)
   indent-bars-pattern "."
   indent-bars-width-frac 0.1
   indent-bars-pad-frac 0.1
   indent-bars-zigzag nil
   indent-bars-color-by-depth nil
   indent-bars-highlight-current-depth nil
   indent-bars-display-on-blank-lines nil)) ; or whichever modes you prefer

(use-package hl-todo
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
  ;; Example: TODO: FIXME: BUG: HACK: TIP: REVIEW: CHECK: NOTE: INSIGHT: DEPRECATED:
  (setq hl-todo-require-punctuation t
        hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("BUG"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("TIP"       font-lock-constant-face bold)
          ("INSIGHT"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("CHECK"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold)))
  :config
  (defun hl-todo-rg (regexp &optional files dir)
    "Use `rg' to find all TODO: or similar keywords."
    (interactive
     (progn
       (unless (require 'rg nil t)
         (error "`rg' is not installed"))
       (let ((regexp (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp))))
         (list regexp
               (rg-read-files)
               (read-directory-name "Base directory: " nil default-directory t)))))
    (rg regexp files dir))
  (defun hl-todo-rg-project ()
    "Use `rg' to find all TODO or similar keywords in current project."
    (interactive)
    (unless (require 'rg nil t)
      (error "`rg' is not installed"))
    (rg-project (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp)) "everything")))


(provide 'init-highlight)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-highlight.el ends here
