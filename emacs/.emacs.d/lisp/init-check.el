;;; init-check.el --- Code Checker configurations.	-*- lexical-binding: t -*-

;;; Commentry:
;; setting up code-checking

;;; Code:

(use-package flymake
  :ensure nil
  :hook (prog-mode . (lambda ()
                       (unless (derived-mode-p 'racket-mode)
                         (flymake-mode))))
  :config
  (setq flymake-no-changes-timeout nil
        flymake-fringe-indicator-position 'right-fringe)

  (setq flymake-mode-line-lighter 
      (propertize (concat "F" (char-to-string #x2107))
                  'face '(:weight bold))))

(use-package sideline-flymake
  :ensure t
  :diminish sideline-mode
  :custom-face
  (sideline-flymake-error ((t (:height 0.85 :italic t))))
  (sideline-flymake-warning ((t (:height 0.85 :italic t))))
  (sideline-flymake-success ((t (:height 0.85 :italic t))))
  :hook (flymake-mode . sideline-mode)
  :init (setq sideline-flymake-display-mode 'point
              sideline-backends-right '(sideline-flymake)))

(provide 'init-check)
;;; init-check.el ends here
