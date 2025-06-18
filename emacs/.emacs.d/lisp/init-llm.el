;;; init-llm.el --- summary -*- lexical-binding: t -*-


;;; Commentary:

;;; Code:
(use-package gptel
  :disabled
  :init
  ;; OPTIONAL configuration
  (setq gptel-model   'deepseek-chat
        gptel-backend
        (gptel-make-openai "DeepSeek"     ;Any name you want
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :stream t
          :key "sk-73be20837e3746b4986fda6c986674b6"             ;can be a function that returns the key
          :models '(deepseek-chat deepseek-coder)))
  )


(provide 'init-llm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-llm.el ends here
