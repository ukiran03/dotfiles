;;; init-modeline.el --- Modeline configurations.	-*- lexical-binding: t -*-

;; Commentry:
;; Modeline settings

;; Code

(defvar mode-line-cleaner-alist
  '((lisp-interaction-mode . "λ")
    (python-mode . "Py")
    (emacs-lisp-mode . "Eλ")
    (sh-mode . "SH")
    (puni-mode . "⟮⟯")
    (racket-mode . "RKT")
    (scheme-mode . "λSCM"))
  "Alist for `clean-mode-line'.

  ; ;; When you add a new element to the alist, keep in mind that you
  ; ;; must pass the correct minor/major mode symbol and a string you
  ; ;; want to use in the modeline *in lieu of* the original.")
(defun clean-mode-line ()
  (cl-loop for cleaner in mode-line-cleaner-alist
           do (let* ((mode (car cleaner))
                     (mode-str (cdr cleaner))
                     (old-mode-str (cdr (assq mode minor-mode-alist))))
                (when old-mode-str
                  (setcar old-mode-str mode-str))
                ;; major mode
                (when (eq mode major-mode)
                  (setq mode-name mode-str)))))
(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(provide 'init-modeline)

;;; init-modeline.el ends here
