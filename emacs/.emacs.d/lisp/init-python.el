;;; init-python.el --- Initialize Python configurations. -*- lexical-binding: t -*-

;;; Commentary:
;; Sets up Python environment defaults with modern Tree-sitter configurations,
;; automatic `uv` virtual environment activation, code formatting, and a custom
;; Manim scene renderer.

;;; Code:

;; -- Python setup with tree-sitter
(use-package python-base-mode
  :ensure nil
  :hook ((python-base-mode . apheleia-mode)
         (python-base-mode . symbol-overlay-mode)
         (python-base-mode . uv-mode-auto-activate-hook))
  :custom
  (python-indent-offset 4))

(use-package python-ts-mode
  :ensure nil
  ;; :hook ((python-base-mode . eglot-ensure))
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode))

;; Remap all python buffers to tree-sitter mode
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(use-package uv-mode
  :ensure t)

(defun manim-render-scene ()
  "Invoke the manim command with the current file and the selected region.
Uses Emacs' compile function and unmarks the region after invoking."
  (interactive)
  (if (use-region-p)
      (let* ((region-text
              (buffer-substring-no-properties (region-beginning) (region-end)))
             (file (buffer-file-name))
             (command (if (and region-text file)
                          (concat "uv run manim -pql "
                                  (shell-quote-argument file)
                                  " "
                                  (shell-quote-argument region-text))
                        (error "No region or current file validly detected"))))
        ;; Unmark the region (deactivate the selection)
        (deactivate-mark)
        ;; Use the built-in compile function
        (compile command))
    (error "No active region selected")))

(provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
