;;; init-eshell.el --- summary -*- lexical-binding: t -*-


;;; Commentary:

;;; Code:
(use-package eshell-prompt-extras
  :init
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)))

(setq-default explicit-shell-file-name "/usr/bin/bash")
(setq shell-file-name "bash")

;; (setq explicit-zsh-args '("--login" "--interactive"))
;; (defun zsh-shell-mode-setup ()
;;   (setq-local comint-process-echoes t))
;; (add-hook 'shell-mode-hook #'zsh-shell-mode-setup)

;; https://karthinks.com/software/jumping-directories-in-eshell/
(defun eshell/z (&optional regexp)
  "Navigate to a previously visited directory in eshell, or to
any directory proferred by `consult-dir'."
  (let ((eshell-dirs (delete-dups
                      (mapcar 'abbreviate-file-name
                              (ring-elements eshell-last-dir-ring)))))
    (cond
     ((and (not regexp) (featurep 'consult-dir))
      (let* ((consult-dir--source-eshell `(:name "Eshell"
                                                 :narrow ?e
                                                 :category file
                                                 :face consult-file
                                                 :items ,eshell-dirs))
             (consult-dir-sources (cons consult-dir--source-eshell
                                        consult-dir-sources)))
        (eshell/cd (substring-no-properties
                    (consult-dir--pick "Switch directory: ")))))
     (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                     (completing-read "cd: " eshell-dirs)))))))

(provide 'init-eshell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eshell.el ends here
