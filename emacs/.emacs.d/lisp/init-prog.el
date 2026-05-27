;;; init-prog.el --- Programming Configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(add-to-list 'auto-mode-alist '("xresources\\'" . conf-xdefaults-mode))

(use-package re-builder
  :ensure nil
  :config (setq reb-re-syntax 'rx)) ; I love using rx for regexps

;; Tree-sitter setup (Kept commented out as per original layout)
;; (when (uk/treesit-available-p)
;;   (use-package treesit-auto
;;     :hook (after-init . global-treesit-auto-mode)
;;     :init (setq treesit-auto-install 'prompt))
;;
;;   (use-package treesit-fold-indicators
;;     :ensure treesit-fold
;;     :hook (after-init . global-treesit-fold-indicators-mode)
;;     :init (setq treesit-fold-indicators-priority -1)))

;; dtrt (auto find indent setting)
(use-package dtrt-indent
  :config (dtrt-indent-global-mode))

(use-package emacs
  :ensure nil
  :config
  (defun my-compile-goto-error-recenter (&rest _args)
    "Recenter window after `compile-goto-error`."
    (recenter-top-bottom))
  (advice-add 'compile-goto-error :after #'my-compile-goto-error-recenter)
  (setq warning-minimum-level :error))

(use-package pretty-hydra
  :disabled
  :bind ("C-<f5>" . toggles-hydra/body)
  :init
  ;; Global toggles
  (with-no-warnings
    (pretty-hydra-define
      toggles-hydra (:color amaranth :quit-key ("q" "C-g"))
      ("Basic"
       (("a" aggressive-indent-mode "Ag indent" :toggle t)
        ("p" puni-mode "Puni" :toggle t)
        ("F" format-all-mode "FMT" :toggle t))
       "Highlight"
       (("h s" symbol-overlay-mode "symbol OL" :toggle t)
        ("h t" global-hl-todo-mode "todo" :toggle t)
        ("c" colorful-mode "colorful" :toggle t))
       "Program"
       (("f" flymake-mode "flymake" :toggle t)
        ("d" diff-hl-mode "diff-hl" :toggle t)
        ("D" diff-hl-dired-mode "dired diff-hl" :toggle t))))))

;; Emacs 30.1+ Built-in completion preview
(use-package completion-preview
  :ensure nil
  :blackout
  :hook (prog-mode . completion-preview-mode))

(use-package newcomment
  :ensure nil
  :bind ("H-;" . comment-line))

(use-package subword
  :ensure nil
  :blackout subword-mode
  :hook (prog-mode . subword-mode))

(use-package page-break-lines
  :blackout
  :ensure t
  :hook (prog-mode . page-break-lines-mode))

(use-package eldoc
  :ensure nil
  :blackout eldoc-mode
  :config
  (setq eldoc-echo-area-use-multiline-p t))

;; Format-all (Disabled in favor of apheleia)
(use-package format-all
  :disabled
  :blackout
  :commands format-all-mode
  :config
  (setq format-all-formatters
        '(("C"       (astyle "--mode=c"))
          ("C"       (astyle "--style=gnu"))
          ("Shell"   (shfmt "-i" "4" "-ci"))
          ("Haskell" (hindent)))))

(use-package apheleia
  :ensure t
  :config
  (setq apheleia-mode-lighter " ;")
  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format" "--style={IndentWidth: 4}"))
  (setf (alist-get 'hindent apheleia-formatters)
        '("hindent"))
  (setf (alist-get 'haskell-mode apheleia-mode-alist) '(hindent))
  (setf (alist-get 'elm-mode apheleia-mode-alist) '(elm-format)))

;; Click to browse URL or to send to e-mail address
(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

;; Abbreviations with abbrev.el
(use-package abbrev
  :ensure nil
  :hook ((text-mode . abbrev-mode)
         (prog-mode . abbrev-mode))
  :config
  (define-abbrev prog-mode-abbrev-table "tmc" "time complexity:")
  (define-abbrev prog-mode-abbrev-table "spc" "space complexity:"))

;; Structural editing / Delimiter management
(use-package paredit :ensure nil)

(use-package puni
  :ensure t
  :hook (((prog-mode racket-repl-mode eval-expression-minibuffer-setup-hook) . puni-mode)
         ((calc-mode term-mode vterm-mode info-mode vertico-mode) . puni-disable-puni-mode))
  :bind (("C-c s" . puni-mode)
         :map puni-mode-map
         ([remap backward-sentence] . puni-end-of-sexp)
         ([remap forward-sentence] . puni-beginning-of-sexp)
         ([remap forward-sexp] . puni-forward-sexp-or-up-list)
         ([remap backward-sexp] . puni-backward-sexp-or-up-list)
         ([remap kill-line] . puni-kill-line)
         ([remap mark-paragraph] . puni-expand-region)
         ("M-k" . puni-raise)
         ("M-O" . puni-splice)
         ("C-)" . puni-slurp-forward)
         ("C-(" . puni-slurp-backward)
         ("C-}" . puni-barf-forward)
         ("C-{" . puni-barf-backward)
         ("M-(" . puni-wrap-round)
         ("M-C" . puni-clone-thing-at-point)
         ("C-M-t" . puni-transpose)
         ("C-M-?" . puni-convolute)
         ("C-M-z" . puni-squeeze)
         ("M-<backspace>" . backward-kill-word)
         ("C-w" . kill-region))
  :config
  (setopt puni-blink-region-face 'show-paren-match))

(use-package aggressive-indent
  :blackout
  :ensure t
  :hook ((emacs-lisp-mode lisp-interaction-mode racket-mode scheme-mode) . aggressive-indent-mode))

(use-package indent-bars
  :config
  (setq indent-bars-color '(highlight :face-bg t :blend 0.2)
        indent-bars-pattern "."
        indent-bars-width-frac 0.1
        indent-bars-pad-frac 0.1
        indent-bars-zigzag nil
        indent-bars-color-by-depth nil
        indent-bars-highlight-current-depth nil
        indent-bars-display-on-blank-lines nil))

;; API Testing Packages
(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode)))

(use-package verb
  :ensure t
  :config
  (setq verb-auto-kill-response-buffers t))

(use-package files
  :ensure nil
  :bind (:map prog-mode-map
              ("S-<f5>" . revert-buffer)))

;; SQL Configuration
(use-package sql
  :ensure nil
  :defer t
  :config
  (setq sql-sqlite-options '("-header" "-box")
        sql-postgres-login-params '(server port user database))) ; Corrected parameter tokens

;; Compilation Buffers (Merged blocks)
(use-package compile
  :ensure nil
  :config
  ;; Colorize compilation output
  (with-eval-after-load 'ansi-color
    (defun endless/colorize-compilation ()
      "Colorize from `compilation-filter-start' to `point'."
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region compilation-filter-start (point))))
    (add-hook 'compilation-filter-hook #'endless/colorize-compilation))

  ;; Auto-detect local C file compilation command
  (defun set-c-compile-command ()
    (setq-local compile-command
                (format "gcc %s" (file-name-nondirectory buffer-file-name))))
  :hook ((c-mode . set-c-compile-command)
         (c-ts-mode . set-c-compile-command)))

;; Context-aware tags navigation helper
(use-package clue
  :ensure nil
  :defer t
  :vc (:url "https://github.com/AmaiKinono/clue")
  :blackout (clue-mode . " #")
  :config
  (setq clue-auto-enable-modes '(prog-mode)))

;; Debug Adapter Protocol (DAP)
(use-package dape
  :config
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)
  (remove-hook 'dape-start-hook 'dape-repl))

;; Direnv Integration
(use-package envrc
  :ensure t
  :config
  (setq envrc-none-lighter nil
        envrc-on-lighter '(:propertize " env" face envrc-mode-line-on-face)
        envrc-error-lighter '(:propertize " env!" face (envrc-mode-line-error-face bold)))
  :hook (after-init . envrc-global-mode)
  :bind (:map envrc-mode-map
              ("C-c e" . envrc-command-map)))

(provide 'init-prog)
;;; init-prog.el ends here
