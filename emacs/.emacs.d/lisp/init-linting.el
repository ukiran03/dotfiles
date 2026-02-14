;;; init-check.el --- Code Checker configurations.	-*- lexical-binding: t -*-

(use-package flymake
  :ensure nil
  :hook (elisp-mode . flymake-mode)
  :config
  (setq flymake-no-changes-timeout nil)
  (setq flymake-fringe-indicator-position 'right-fringe)
  (setq flymake-mode-line-lighter "FY"))

;; flymake to check only after file save
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

;; stackoverflow.com/questions/6110691
;; (use-package emacs
;;   :ensure nil
;;   :config
;;   (eval-after-load "flymake"
;;     '(progn
;;        (defun flymake-after-change-function (start stop len)
;;          "Start syntax check for current buffer if it isn't already running."
;;          ;; Do nothing, don't want to run checks until I save.
;;          ))))

(use-package jinx
  :ensure t
  :blackout
  ;; :hook
  ;; ((prog-mode-hook
  ;;   conf-mode-hook
  ;;   text-mode-hook) . jinx-mode)
  :bind
  (([remap ispell-word] . jinx-correct)
   (:repeat-map jinx-repeat-map
                ("s" . jinx-next)
                ("S" . jinx-previous)))
  ;; :init
  ;; (keymap-set next-map "s" '("Misspelling" . jinx-next))
  ;; (keymap-set prev-map "s" '("Misspelling" . jinx-previous))
  )

(use-package dictionary
  :ensure nil
  :init
  ;; (setq dictionary-server "localhost") -- TODO: setup dictd on void
  )

(provide 'init-linting)
