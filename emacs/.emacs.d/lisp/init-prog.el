;;; init-prog.el --- summary -*- lexical-binding: t -*-


;;; Commentary:

;;; Code:

(add-to-list 'auto-mode-alist '("xresources\\'" . conf-xdefaults-mode))

(use-package re-builder
  :ensure nil
  :config (setq reb-re-syntax 'rx)) ;I love using rx for regexps


(use-package pretty-hydra
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

;; Emacs 30.1
(use-package completion-preview
  :ensure nil
  :diminish
  :hook
  (prog-mode . completion-preview-mode))

(use-package newcomment
  :ensure nil
  :bind ("H-;" . comment-line))

(use-package subword
  :ensure nil
  :diminish subword-mode
  :hook (prog-mode . subword-mode))

(use-package page-break-lines
  :diminish
  :ensure t
  :hook (prog-mode . page-break-lines-mode))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode
  :config
  (setq eldoc-echo-area-use-multiline-p t))

;;  `C': <https://astyle.sourceforge.net/astyle.html>
(use-package format-all
  ;; :disabled
  :diminish
  :commands format-all-mode
  ;; :hook (prog-mode . format-all-mode)
  :config
  (setq format-all-formatters
        '(("C"     (astyle "--mode=c"))
          ("C" (astyle "--style=gnu"))
          ("Shell" (shfmt "-i" "4" "-ci"))
          ("Haskell" (hindent)))))

(use-package apheleia
  :disabled
  :config
  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format" "--style={IndentWidth: 4}")))

;; Similar to format-all
;; checkout: <https://github.com/purcell/emacs-reformatter>

;; Click to browse URL or to send to e-mail address
(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

;; ‘Animated-Guide’ : <http://danmidwood.com/content/2014/11/21/animated-paredit.html>
(use-package paredit :ensure nil)

;;;; Setup Folding For Programming
(use-package puni
  :ensure t
  :hook (((prog-mode racket-repl-mode eval-expression-minibuffer-setup-hook) . puni-mode)
         ((calc-mode term-mode vterm-mode info-mode vertico-mode) . puni-disable-puni-mode)
         ;; (puni-mode  . electric-pair-local-mode)
         )
  :bind (("C-c s" . puni-mode)
         :map puni-mode-map
         ([remap backward-sentence] . puni-end-of-sexp)
         ([remap forward-sentence]  . puni-beginning-of-sexp)
         ([remap forward-sexp]      . puni-forward-sexp-or-up-list)
         ([remap backward-sexp]     . puni-backward-sexp-or-up-list)
         ([remap kill-line]        . puni-kill-line)
         ([remap mark-paragraph]    . puni-expand-region)
         ;; ([remap kill-sexp]        . puni-kill-thing-at-point)
         ("M-k"                     . puni-raise)
         ;; Remove outer pairs
         ("M-O"                     . puni-splice)
         ("C-)"                     . puni-slurp-forward)
         ("C-("                     . puni-slurp-backward)
         ("C-}"                     . puni-barf-forward)
         ("C-{"                     . puni-barf-backward)
         ("M-("                     . puni-wrap-round)
         ("M-C"                     . puni-clone-thing-at-point)
         ("C-M-t"                   . puni-transpose)
         ("C-M-?"                   . puni-convolute)
         ("C-M-z"                   . puni-squeeze)
         ("M-<backspace>"           . backward-kill-word)
         ("C-w" . kill-region))
  :config
  (setopt puni-blink-region-face 'show-paren-match))

(use-package aggressive-indent
  :diminish
  :ensure t
  :hook ((emacs-lisp-mode
          lisp-interaction-mode
          racket-mode
          scheme-mode) . aggressive-indent-mode))

(use-package emacs
  :ensure nil
  :config
  (setq warning-minimum-level :error))

;; <https://erick.navarro.io/blog/testing-an-api-with-emacs-and-restclient/>
(use-package restclient
  :ensure t
  :mode (("\\.http\\'" . restclient-mode)))

(use-package files
  :ensure nil
  :bind (:map prog-mode-map
              ("<f5>" . revert-buffer)))

(use-package sql
  :ensure nil
  :defer
  :custom
  (sql-sqlite-options '("-header" "-box"))
  )

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
