;;; init-linting.el --- Code Linting configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;; Configures Flymake, Eglot diagnostic save hooks, Jinx spell checking,
;; and dictionary integrations.

;;; Code:

(use-package flymake
  :straight (:type built-in)
  :hook (elisp-mode . flymake-mode)
  :config
  (setq flymake-no-changes-timeout nil)
  (setq flymake-fringe-indicator-position 'right-fringe)
  (setq flymake-mode-line-lighter "fly"))

;; Flymake to check only after file save
;; github.com/joaotavora/eglot/issues/1296#issuecomment-1727978307
;; sasha.ovh/wiki/emacs/Make_flymake_only_check_for_errors_on_save_including_Eglot/
(cl-defmethod eglot-handle-notification :after
  (_server (_method (eql textDocument/publishDiagnostics)) &key uri
           &allow-other-keys)
  (when-let ((buffer (find-buffer-visiting (eglot-uri-to-path uri))))
    (with-current-buffer buffer
      (if (and (eq nil flymake-no-changes-timeout)
               (not (buffer-modified-p)))
          (flymake-start t)))))

(use-package jinx
  :straight t
  :bind
  (([remap ispell-word] . jinx-correct)
   (:repeat-map jinx-repeat-map
                ("s" . jinx-next)
                ("S" . jinx-previous))))

(use-package dictionary
  :straight (:type built-in)
  :init
  ;; (setq dictionary-server "localhost") -- TODO: setup dictd on void
  )

(provide 'init-linting)
;;; init-linting.el ends here
