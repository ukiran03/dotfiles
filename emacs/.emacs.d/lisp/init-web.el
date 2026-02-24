;; Base JS mode (for shared hooks)
(use-package js
  :ensure nil ;; built-in
  :hook ((js-base-mode . apheleia-mode)
         (js-base-mode . symbol-overlay-mode))
  :custom
  (js-indent-level 2)
  :config
  (add-to-list 'interpreter-mode-alist '("node" . js-base-mode)))

;; JS (tree-sitter) mode for .js/.mjs files
(use-package js-ts-mode
  :ensure nil
  :mode ("\\.m?js\\'" . js-ts-mode)
  :custom
  (js-ts-indent-offset 2))

;; JSX mode for .jsx files
(use-package js-jsx-mode
  :ensure nil
  :mode ("\\.jsx\\'" . js-jsx-mode)
  :custom
  (js-indent-level 2))

(use-package typescript-ts-base-mode
  :ensure nil
  :hook ((typescript-ts-base-mode . apheleia-mode)
         (typescript-ts-base-mode . symbol-overlay-mode))
  :custom
  (typescript-ts-mode-indent-offset 2))

(use-package typescript-ts-mode
  :ensure nil
  :mode ("\\.ts\\'" . typescript-ts-mode)
  :custom
  (typescript-ts-mode-indent-offset 2))

(use-package tsx-ts-mode
  :ensure nil
  :mode ("\\.tsx\\'" . tsx-ts-mode)
  :custom
  (typescript-ts-mode-indent-offset 2))

;; Emmet for JSX/TSX
(use-package emmet-mode
  :hook ((js-jsx-mode . emmet-mode)
         (tsx-ts-mode . emmet-mode)
         (web-mode   . emmet-mode))
  :blackout (emmet-mode . " </>")
  :bind
  (:map emmet-mode-keymap
        ("C-<return>" . nil))
  :config
  (setq emmet-expand-jsx-className? t))
                                        ; Web-Mode
(use-package web-mode
  :mode "\\.html\\'"
  :hook ((web-mode . emmet-mode)
         ;; (web-mode . apheleia-mode)
         )
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package web-mode
  :mode "\\.tmpl\\'"
  :hook ((web-mode . emmet-mode)
         (web-mode . (lambda ()
                       ;; (when (and (buffer-file-name)
                       ;;            (string-equal
                       ;;             (file-name-extension (buffer-file-name)) "tmpl"))
                       ;;   __)
                       (setq-local electric-pair-inhibit-predicate
                                   (lambda (char)
                                     (if (char-equal char ?{)
                                         t
                                       (electric-pair-default-inhibit char)))))))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2))

;; Prettier-Js
(use-package prettier-js
  :disabled
  :blackout (prettier-js-mode . " ;")
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "all"
                           "--bracket-spacing" "true"
                           "--tabWidth" "4"
                           "--useTabs" "false"
                           "--semi" "true"
                           "--singleQuote" "true"
                           )))


;;; VueJS3

;; (use-package eglot
;;   :ensure t
;;   :config
;;   ;; Link web-mode to your 'vls' or 'vue-language-server'
;;   (add-to-list 'eglot-server-programs
;;                `(web-mode . ("vue-language-server" "--stdio"
;;                              :initializationOptions
;;                              (:vue (:hybridMode :json-false)
;;                                    ;; Tells Volar we are JS-only
;;                                    :typescript (:serverPath "")))))
;;   ;; Auto-start Eglot when you open your Vue components
;;   (add-hook 'web-mode-hook 'eglot-ensure)
;;   (setq eglot-autostart-session 'every-buffer))

;; (use-package web-mode
;;   :ensure t
;;   :mode "\\.vue\\'"
;;   :config
;;   ;; Standard Vue 2-space indentation
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2)
;;   ;; This makes sure web-mode treats the script block as JS
;;   (setq web-mode-content-types-alist '(("jsx" . "\\.vue\\'"))))

(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :hook ((vue-mode . emmet-mode)
         (mmm-mode . (lambda ()
                       (set-face-background 'mmm-default-submode-face nil)))))

(provide 'init-web)
