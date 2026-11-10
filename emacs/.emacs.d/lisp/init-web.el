;;; init-web.el --- Web development configuration -*- lexical-binding: t -*-

;;; Commentary:
;; JavaScript, TypeScript, HTML, CSS, and Web-mode configurations.

;;; Code:

;; JavaScript & TypeScript (Modern Tree-Sitter)

;; Base JS mode (for shared hooks)
(use-package js
  :straight (:type built-in)
  :custom
  (js-indent-level 4)
  :hook
  ((js-base-mode . apheleia-mode)
   (js-base-mode . symbol-overlay-mode))
  :config
  (add-to-list 'interpreter-mode-alist '("node" . js-base-mode)))

;; JS (tree-sitter) mode for .js/.mjs files
(use-package js-ts-mode
  :straight (:type built-in)
  :mode
  ("\\.m?js\\'" . js-ts-mode)
  :custom
  (js-ts-indent-offset 2))

;; JSX mode for .jsx files
(use-package js-jsx-mode
  :straight (:type built-in)
  :mode
  ("\\.jsx\\'" . js-jsx-mode)
  :custom
  (js-indent-level 2))

;; TypeScript Base Mode (for shared hooks)
(use-package typescript-ts-base-mode
  :straight (:type built-in)
  :custom
  (typescript-ts-mode-indent-offset 2)
  :hook
  ((typescript-ts-base-mode . apheleia-mode)
   (typescript-ts-base-mode . symbol-overlay-mode)))

;; TypeScript (tree-sitter) mode for .ts files
(use-package typescript-ts-mode
  :straight (:type built-in)
  :mode
  ("\\.ts\\'" . typescript-ts-mode)
  :custom
  (typescript-ts-mode-indent-offset 2))

;; TSX mode for .tsx files
(use-package tsx-ts-mode
  :straight (:type built-in)
  :mode
  ("\\.tsx\\'" . tsx-ts-mode)
  :custom
  (typescript-ts-mode-indent-offset 2))

;; HTML & Templating (Web-Mode & Emmet)

;; Emmet for HTML/JSX/TSX
(use-package emmet-mode
  :straight t
  :custom
  (emmet-expand-jsx-className? t)
  :hook
  ((js-jsx-mode . emmet-mode)
   (tsx-ts-mode . emmet-mode)
   (web-mode    . emmet-mode))
  :bind
  (:map emmet-mode-keymap
        ("C-<return>" . nil)))

;; Web-Mode for HTML, Go templates, and Vue
(use-package web-mode
  :straight t
  :mode
  (("\\.html\\'" . web-mode)
   ("\\.tmpl\\'" . web-mode)
   ("\\.vue\\'"  . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-content-types-alist '(("jsx" . "\\.vue\\'")))
  :hook
  ((web-mode . emmet-mode)
   ;; (web-mode . apheleia-mode)
   (web-mode . (lambda ()
                 ;; Inhibit matching pairs for Go template braces
                 (when (and (buffer-file-name)
                            (string-equal
                             (file-name-extension (buffer-file-name)) "tmpl"))
                   (setq-local electric-pair-inhibit-predicate
                               (lambda (char)
                                 (if (char-equal char ?{)
                                     t
                                   (electric-pair-default-inhibit char)))))))))

;; Language Server Protocol (LSP via Eglot)

;; Vue 3 LSP support using Volar
(use-package eglot
  :straight (:type built-in)
  :custom
  (eglot-autostart-session 'every-buffer)
  :hook
  (web-mode . (lambda ()
                (when (and (buffer-file-name)
                           (string-equal
                            (file-name-extension (buffer-file-name)) "vue"))
                  (eglot-ensure))))
  :config
  ;; Link web-mode Vue buffers to vue-language-server
  (add-to-list 'eglot-server-programs
               `(web-mode . ("vue-language-server" "--stdio"
                             :initializationOptions
                             (:vue (:hybridMode :json-false)
                                   :typescript (:serverPath ""))))))

;; Disabled Packages / Legacy

;; Prettier-Js (Using apheleia-mode instead via hooks)
(use-package prettier-js
  :disabled t
  :straight t
  :custom
  (prettier-js-args '("--trailing-comma" "all"
                      "--bracket-spacing" "true"
                      "--tabWidth" "4"
                      "--useTabs" "false"
                      "--semi" "true"
                      "--singleQuote" "true")))

(provide 'init-web)
;;; init-web.el ends here
