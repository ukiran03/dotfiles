;;; init-dape.el --- Debug Adapter Protocol Integration -*- lexical-binding: t -*-

(use-package dape
  :straight t
  :custom
  ;; Performance tweaks
  (gc-cons-threshold 80000000)

  ;; Window layout & inlay hints
  (dape-buffer-window-arrangement 'right)
  (dape-inlay-hints-variable-name-max 50)
  (dape-inlay-hints nil)

  :hook
  ;; Pulse source line on display
  (dape-display-source-hook . pulse-momentary-highlight-one-line)

  :config
  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-start-hook #'dape-info)
  (remove-hook 'dape-start-hook #'dape-repl)

  ;; Add the Go delve configuration if not already present
  (add-to-list 'dape-configs
               `(go-debug-main
                 modes (go-mode go-ts-mode)
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1::autoport")
                 command-cwd dape-command-cwd
                 port :autoport
                 :type "debug"
                 :request "launch"
                 :name "Debug Go Program"
                 :cwd "."
                 :program "."
                 :args [])))

(provide 'init-dape)
;;; init-dape.el ends here
