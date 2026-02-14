;; init-python.el --- Initialize Python configurations.	-*- lexical-binding: t -*-

;; -- Python setup with tree-sitter
(use-package python-base-mode
  :ensure nil
  :hook ((python-base-mode . apheleia-mode)
         (python-base-mode . symbol-overlay-mode))
  :custom
  (python-indent-offset 4))

(use-package python-ts-mode
  :ensure nil
  ;; :hook ((python-base-mode . eglot-ensure))
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode))

;; Remap all python buffers to tree-sitter mode
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; (use-package pythonic)
(use-package uv-mode
  :hook (python-mode . uv-mode-auto-activate-hook))

(defun manim-render-scene ()
  "Invoke the manim command with the current file and the selected region using Emacs' compile function. Unmark the region after invoking."
  (interactive)
  (if (use-region-p)  ; Check if there's an active region
      (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end)))  ; Get the region's content
             (file (buffer-file-name))  ; Get the current file's name
             (command (if (and region-text file)
                          (concat "uv run manim -pql " file " " region-text)
                        (error "No region or current file"))))
        ;; Unmark the region (deactivate the selection)
        (deactivate-mark)
        ;; Use the built-in compile function
        (compile command))
    (error "No active region selected")))

(provide 'init-python)
