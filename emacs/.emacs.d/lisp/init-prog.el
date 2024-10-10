(use-package subword
  :ensure nil
  :diminish subword-mode
  :hook (prog-mode . subword-mode)
  ;; :config
  ;; (diminish 'subword-mode
  ;; '(:propertize " SUBW" face '(:foreground "blue")))
  )

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
  :diminish
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq format-all-formatters
        '(("C"     (astyle "--mode=c"))
      ("C" (astyle "--style=gnu"))
      ("Shell" (shfmt "-i" "4" "-ci")))))

;; Similar to format-all
;; checkout: <https://github.com/purcell/emacs-reformatter>

;; Click to browse URL or to send to e-mail address
(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

(use-package smartparens
  ;; <https://ebzzry.com/en/emacs-pairs/>
  :disabled
  :diminish
  :ensure smartparens  ;; install the package
  :hook (prog-mode ;; add `smartparens-mode` to these hooks
         geiser-repl-mode
         racket-repl-mode
         text-mode
         markdown-mode)
  :config
  ;; load default config
  (require 'smartparens-config))

;; ‘Animated-Guide’ : <http://danmidwood.com/content/2014/11/21/animated-paredit.html>
(use-package paredit :ensure nil)

;;;; Setup Folding For Programming
(use-package puni
  :ensure t
  :hook (((calc-mode term-mode vterm-mode info-mode) . puni-disable-puni-mode)
         ((prog-mode racket-repl-mode eval-expression-minibuffer-setup-hook) . puni-mode)
         (puni-mode  . electric-pair-local-mode))
  :bind (("C-c s" . puni-mode)
         :map puni-mode-map
         ("C-c DEL"                 . jinx-correct)
         ([remap backward-sentence] . puni-end-of-sexp)
         ([remap forward-sentence]  . puni-beginning-of-sexp)
         ([remap forward-sexp]      . puni-forward-sexp-or-up-list)
         ([remap backward-sexp]     . puni-backward-sexp-or-up-list)
         ([remap kill-line]        . puni-kill-line)
         ([remap mark-paragraph]    . puni-expand-region)
         ([remap kill-sexp]        . puni-kill-thing-at-point)
         ("M-k"                     . kill-sexp)
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
  ;; (puni-global-mode t))


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

(provide 'init-prog)
