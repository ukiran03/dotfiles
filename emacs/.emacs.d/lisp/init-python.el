;;; init-python.el --- Initialize Python configurations. -*- lexical-binding: t -*-

;;; Commentary:
;; Sets up Python environment defaults with modern Tree-sitter configurations
;; and code formatting.

;;; Code:

(use-package python
  :straight nil
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode)
  :hook ((python-ts-mode . apheleia-mode)
         (python-ts-mode . eglot-ensure)
         (python-ts-mode . indent-bars-mode)
         (python-ts-mode . symbol-overlay-mode))
  :config
  ;; Map standard python-mode to python-ts-mode if tree-sitter is available
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))


(provide 'init-python)
;;; init-python.el ends here
