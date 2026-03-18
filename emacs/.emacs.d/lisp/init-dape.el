(use-package dape
  :ensure t
  :init
  ;; Performance tweaks
  (setq gc-cons-threshold 80000000)

  :config
  (setq dape-buffer-window-arrangement 'right)
  (setq dape-inlay-hints-variable-name-max 50)
  (setq dape-inlay-hints nil)
  :config
  ;; Pulse source line (performance hit)
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)
  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-start-hook 'dape-info)
  (remove-hook 'dape-start-hook 'dape-repl)

  :config
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
