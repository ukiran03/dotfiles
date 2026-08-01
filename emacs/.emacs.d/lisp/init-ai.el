;;; init-ai.el --- AI interactions -*- lexical-binding: t -*-

(use-package gptel
  :straight t
  :config
  (setq gptel-api-key (getenv "GEMINI_API_KEY"))

  ;; Create and assign the Gemini backend with its model pool mapped
  (setq gptel-backend (gptel-make-gemini "Gemini"
                        :key gptel-api-key
                        :stream t
                        :models '("gemini-2.5-flash" "gemini-2.5-pro")))
  (setq gptel-default-mode 'org-mode)
  ;; Now safely set your default global model
  (setq gptel-model "gemini-2.5-flash")

  (add-hook 'gptel-post-response-functions 'visual-line-mode))

;; :bind (("C-c g g" . gptel-menu) ; Transients menu for switching
;; models/system prompts
;; ("C-c g i" . gptel)       ; Start a dedicated chat buffer
;; ("C-c g s" . gptel-send)) ; Send active region or current prompt

(provide 'init-ai)
;;; init-ai.el ends here
