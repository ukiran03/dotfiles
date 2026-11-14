;;; init-java.el --- Java Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Java development environment configurations, compilation tools,
;; and formatting setup.

;;; Code:

(use-package cc-mode
  :straight (:type built-in)
  :preface
  (defun java-compile-command ()
    "Compile command to run the current Java file."
    (interactive)
    (let* ((file (buffer-file-name))
           (filename (and file (file-name-nondirectory file)))
           (basename (and file (file-name-base file))))
      (if (not file)
          (message "Buffer is not visiting a file.")
        ;; Set the buffer-local or global compile-command default
        (setq compile-command (format "javac %s && java %s" filename basename))
        ;; Call compile interactively so it prompts you with the command
        ;; pre-filled
        (call-interactively #'compile))))

  :hook
  ((java-mode . symbol-overlay-mode)
   (java-mode . puni-mode)
   (java-mode . apheleia-mode)
   (java-mode . (lambda () (setq c-basic-offset 2))))

  :bind
  (:map java-mode-map
        ("<f1>" . java-compile-command)))

;; (use-package apheleia
;;   :straight t
;;   :config
;;   ;; Register the formatter name and its shell command properly
;;   (setf (alist-get 'google-java-format apheleia-formatters)
;;         '("google-java-format" "-"))

;;   ;; Map java-mode to use that formatter symbol
;;   (setf (alist-get 'java-mode apheleia-mode-alist)
;;         'google-java-format))

(use-package apheleia
  :straight t
  :config
  ;; Define the custom formatter as a list of strings
  (setf (alist-get 'clang-java-format apheleia-formatters)
        '("clang-format" "--style={BasedOnStyle: Google, ColumnLimit: 79}" "-"))

  ;; Map java-mode to use your custom formatter symbol
  (setf (alist-get 'java-mode apheleia-mode-alist)
        'clang-java-format))


(provide 'init-java)
;;; init-java.el ends here
