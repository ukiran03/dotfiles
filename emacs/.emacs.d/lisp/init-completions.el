
;; A few more useful configurations...
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Only list the commands of the current modes
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3))

(use-package mb-depth
  :ensure nil
  :hook (after-init . minibuffer-depth-indicate-mode)
  :config
  (setq read-minibuffer-restore-windows nil) ; Emacs 28
  (setq enable-recursive-minibuffers t))

(use-package recursion-indicator
  :ensure t
  :demand t
  :config
  (recursion-indicator-mode))

(use-package minibuffer
  :ensure nil
  :config
  ;;;; Completion styles
  (setq completion-styles '(basic substring initials flex orderless)) ; also see `completion-category-overrides'

  ;; Reset all the per-category defaults so that (i) we use the
  ;; standard `completion-styles' and (ii) can specify our own styles
  ;; in the `completion-category-overrides' without having to
  ;; explicitly override everything.
  (setq completion-category-defaults nil)

    ;; A non-exhaustve list of known completion categories:
  ;;
  ;; - `bookmark'
  ;; - `buffer'
  ;; - `charset'
  ;; - `coding-system'
  ;; - `color'
  ;; - `command' (e.g. `M-x')
  ;; - `customize-group'
  ;; - `environment-variable'
  ;; - `expression'
  ;; - `face'
  ;; - `file'
  ;; - `function' (the `describe-function' command bound to `C-h f')
  ;; - `info-menu'
  ;; - `imenu'
  ;; - `input-method'
  ;; - `kill-ring'
  ;; - `library'
  ;; - `minor-mode'
  ;; - `multi-category'
  ;; - `package'
  ;; - `project-file'
  ;; - `symbol' (the `describe-symbol' command bound to `C-h o')
  ;; - `theme'
  ;; - `unicode-name' (the `insert-char' command bound to `C-x 8 RET')
  ;; - `variable' (the `describe-variable' command bound to `C-h v')
  ;; - `consult-grep'
  ;; - `consult-isearch'
  ;; - `consult-kmacro'
  ;; - `consult-location'
  ;; - `embark-keybinding'
  ;;
(setq completion-category-overrides
        ;; NOTE 2021-10-25: I am adding `basic' because it works better as a
        ;; default for some contexts.  Read:
        ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50387>.
        ;;
        ;; `partial-completion' is a killer app for files, because it
        ;; can expand ~/.l/s/fo to ~/.local/share/fonts.
        ;;
        ;; If `basic' cannot match my current input, Emacs tries the
        ;; next completion style in the given order.  In other words,
        ;; `orderless' kicks in as soon as I input a space or one of its
        ;; style dispatcher characters.
        '((file (styles . (basic partial-completion orderless)))
          (bookmark (styles . (basic substring)))
          (library (styles . (basic substring)))
          (embark-keybinding (styles . (basic substring)))
          (imenu (styles . (basic substring orderless)))
          (consult-location (styles . (basic substring orderless)))
          (kill-ring (styles . (emacs22 orderless)))
          (eglot (styles . (emacs22 substring orderless))))))

(use-package minibuffer
  :ensure nil
  :demand t
  :config
  (setq completions-detailed t)
  (setq completions-max-height 6)
  (setq completions-format 'one-column)
  (setq completion-show-help nil)
  (setq completion-auto-help 'always)
  (setq completion-auto-select nil)
  (setq completions-detailed t)
  (setq completion-show-inline-help nil)
  (setq completions-max-height 6)
    (setq completions-header-format (propertize "%s candidates:\n" 'face 'bold-italic))
  (setq completions-highlight-face 'completions-highlight)
  (setq minibuffer-completion-auto-choose t))

;;;; `savehist' (minibuffer and related histories)
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 100)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-to-list 'savehist-additional-variables 'kill-ring))



(setq completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq-default case-fold-search t)   ; For general regexp
(setq read-file-name-completion-ignore-case t)

(use-package minibuf-eldef
  :ensure nil
  :hook (after-init . minibuffer-electric-default-mode)
  :config
  (setq minibuffer-default-prompt-format " [%s]")) ; Emacs 29



(use-package rfn-eshadow
  :ensure nil
  :hook (minibuffer-setup . cursor-intangible-mode)
  :config
  ;; Not everything here comes from rfn-eshadow.el, but this is fine.

  (setq resize-mini-windows t)
  (setq read-answer-short t) ; also check `use-short-answers' for Emacs28
  (setq echo-keystrokes 0.25)
  (setq kill-ring-max 60) ; Keep it small

  ;; Do not allow the cursor to move inside the minibuffer prompt.  I
  ;; got this from the documentation of Daniel Mendler's Vertico
  ;; package: <https://github.com/minad/vertico>.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (file-name-shadow-mode 1))



;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :demand t
  :after minibuffer
  :config
  ;; Remember to check my `completion-styles' and the
  ;; `completion-category-overrides'.
  (setq orderless-matching-styles
        '(orderless-prefixes orderless-regexp))
    ;; SPC should never complete: use it for `orderless' groups.
  ;; The `?' is a regexp construct.
  :bind ( :map minibuffer-local-completion-map
          ("SPC" . nil)
          ("?" . nil)))


;;; `Code' Completions

;; `tempel.el': Simple templates for Emacs
;; <https://github.com/minad/tempel>
(use-package tempel :ensure nil)
(use-package tempel-collection :ensure nil)
(use-package eglot-tempel :ensure nil)

;; `corfu.el': COmpletion in Region FUnction
;; <https://elpa.gnu.org/packages/doc/corfu.html>
;; <https://github.com/minad/corfu>
  ;; (defun corfu-move-to-minibuffer ()
  ;;   "Move completion to minibuffer instead of corfu."
  ;;   (interactive)
  ;;   (let ((completion-extra-properties corfu--extra)
  ;;         completion-cycle-threshold completion-cycling)
  ;;     (apply #'consult-completion-in-region completion-in-region--data)))
  ;; (define-key corfu-map "\M-m" #'corfu-move-to-minibuffer)

;; `cape.el': Completion At Point Extensions
;; <https://elpa.gnu.org/packages/doc/cape.html>
;; <https://github.com/minad/cape>


;; Auto completion
(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  ;; :custom-face
  ;; (corfu-border ((t (:inherit region :background unspecified))))
  ;; :bind ("M-/" . completion-at-point)
  :bind (:map corfu-map
	      ("M-SPC" . corfu-insert-separator)
	      ("TAB"   . corfu-next)
	      ([tab]   . corfu-next)
	      ("S-TAB" . corfu-previous)
	      ([backtab] . corfu-previous)
	      ("S-<return>" . corfu-insert)
	      ("RET"   . nil)) ;; leave my enter alone
  :config
  (setq corfu-auto nil)
  (setq corfu-auto-prefix 2)
  (setq corfu-preview-current nil)
  ;; (corfu-auto-delay 0.25)
  (setq corfu-min-width 20)
  (setq corfu-popupinfo-delay '(1.25 . 0.5)) ; prev : '(0.4 . 0.2)
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))


;; (unless (display-graphic-p)
;;   (use-package corfu-terminal
;;     :hook (global-corfu-mode . corfu-terminal-mode)))

;; (use-package nerd-icons-corfu
;;   :after corfu
;;   :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; ;; Add extensions
;; (use-package cape
;;   :init
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   (add-to-list 'completion-at-point-functions #'cape-elisp-block)
;;   (add-to-list 'completion-at-point-functions #'cape-keyword)
;;   (add-to-list 'completion-at-point-functions #'cape-abbrev)

;;   (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))


(provide 'init-completions)
