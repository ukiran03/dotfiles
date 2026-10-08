;;; init-java.el --- Java Configuration -*- lexical-binding: t -*-

(use-package java-mode
  :straight nil
  :hook ((java-mode . symbol-overlay-mode)
         (java-mode . puni-mode)
         (java-mode . apheleia-mode)
         (java-mode . (lambda () (setq c-basic-offset 2)))))

(use-package apheleia
  :straight t
  :config
  ;; Register the formatter name and its shell command properly
  (setf (alist-get 'google-java-format apheleia-formatters)
        '("google-java-format" "-"))

  ;; Map java-mode to use that formatter symbol
  (setf (alist-get 'java-mode apheleia-mode-alist)
        'google-java-format))

(provide 'init-java)
;;; init-java.el ends here
