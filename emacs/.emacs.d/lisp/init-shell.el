;;; init-shell.el --- Shell and Eshell configuration -*- lexical-binding: t -*-

;;; Commentary:
;;  Configuration for Comint, Eshell enhancements, and terminal integration.

;;; Code:

;;; Comint & System Shells
(use-package comint
  :ensure nil
  :config
  (setq comint-process-echoes t))

;;; Eshell Prompt Styling
(use-package eshell-prompt-extras
  :ensure t
  :after esh-opt
  :config
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function #'epe-theme-lambda))

;;; Terminal Visual Command Overrides
(use-package ghostel
  :ensure t
  :hook (eshell-load . ghostel-eshell-visual-command-mode))

;;; Eshell Directory Jumping Integration
;;  Ref: https://karthinks.com/software/jumping-directories-in-eshell/
(with-eval-after-load 'eshell
  (declare-function consult-dir--pick "consult-dir")
  (defvar consult-dir-sources)

  (defun eshell/z (&optional regexp)
    "Navigate to a previously visited directory in eshell, or to
any directory proffered by `consult-dir'."
    (let ((eshell-dirs (delete-dups
                        (mapcar #'abbreviate-file-name
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
                       (completing-read "cd: " eshell-dirs))))))))

(provide 'init-shell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
