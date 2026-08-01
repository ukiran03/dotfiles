;;; init-python.el --- Initialize Python configurations. -*- lexical-binding: t -*-

;;; Commentary:
;; Sets up Python environment defaults with modern Tree-sitter configurations
;; and code formatting.

;;; Code:

(use-package python-ts-mode
  :straight nil
  :hook ((python-ts-mode . apheleia-mode)
         (python-ts-mode . eglot-ensure)
         (python-ts-mode . indent-bars-mode)
         (python-ts-mode . symbol-overlay-mode))
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode))

;; Remap all python buffers to tree-sitter mode
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
