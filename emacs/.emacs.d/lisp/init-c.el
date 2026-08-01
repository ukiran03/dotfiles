;;; init-c.el --- C/C++ Configuration -*- lexical-binding: t -*-

;; Legacy CC Mode (Fallback & Shared Variable Initialization)
(use-package cc-mode
  :straight nil
  :init
  (setq-default c-basic-offset 4))

;; Custom function for Treesitter comment styles
(defun my/c-ts-style-settings ()
  "Custom adjustments for Tree-sitter C/C++ modes."
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *"))

;; Modern Tree-sitter C/C++ Mode
(use-package c-ts-mode
  :straight nil ; Built-in
  :init
  (setq c-ts-mode-indent-offset 4)

  ;; Remap standard C/C++ modes to their Tree-sitter counterparts
  (when (boundp 'major-mode-remap-alist)
    (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))

  :bind (:map c-ts-mode-map
              ("<f1>" . compile)
              :map c++-ts-mode-map
              ("<f1>" . compile))

  ;; Apply hooks to both C and C++ Tree-sitter modes
  :hook ((c-ts-mode . symbol-overlay-mode)
         (c-ts-mode . apheleia-mode)
         (c-ts-mode . my/c-ts-style-settings)

         (c++-ts-mode . symbol-overlay-mode)
         (c++-ts-mode . apheleia-mode)
         (c++-ts-mode . my/c-ts-style-settings)))

(provide 'init-c)
