(use-package dape
  :ensure t
  :init
  (setq
   ;; Performance tweaks
   gc-cons-threshold 80000000

   dape-buffer-window-arrangement 'right
   dape-inlay-hints-variable-name-max 50
   dape-inlay-hints nil

   ;; TODO try using :autoport instead of 55878
   dape-configs
   '(
     (go-test
      modes (go-mode go-ts-mode)
      command "dlv"
      command-args ("dap" "--listen" "127.0.0.1:55878")
      command-cwd default-directory
      host "127.0.0.1"
      port 55878
      :name "Run the go tests in cwd"
      :request "launch"
      :mode "test"
      :type "go"
      :program "."
      )

     ;; (go-run-main
     ;;  modes (go-mode go-ts-mode)
     ;;  command "dlv"
     ;;  command-args ("dap" "--listen" "127.0.0.1:55878")
     ;;  ;; command-cwd (project-root (project-current))
     ;;  command-cwd default-directory
     ;;  host "127.0.0.1"
     ;;  port 55878
     ;;  :name "Run main.go with --localtest"
     ;;  :request "launch"
     ;;  :mode "debug"
     ;;  :type "go"
     ;;  :program "main.go"
     ;;  :args ["-localtest"]
     ;;  )
     (go-run-main
      modes (go-mode go-ts-mode)
      command "dlv"
      command-cwd default-directory
      :name "Run main.go with --localtest"
      :request "launch"
      :mode "debug"
      :type "go"
      :program "main.go"
      :args ["-localtest"]
      )
     (go-devspace
      modes (go-mode go-ts-mode)
      host "127.0.0.1"
      port 2345
      prefix-local (lambda () default-directory)
      prefix-remote "/workspace/"
      :name "Debug remote in devspace pod"
      :request "attach"
      :mode "remote"
      :type "go"
      command "dlv"
      command-args ("connect" "127.0.0.1:2345")
      )

     (go-attach
      host "127.0.0.1"
      ;; This is the port exposed in this command:
      ;;
      ;; dlv attach $(lsof -i TCP -s TCP:LISTEN -nP | awk '/:3000/ { print $2 }') --headless --listen=127.0.0.1:5005
      ;; dlv attach 68436 --headless --listen=127.0.0.1:38697 --accept-multiclient --api-version=2 --log
      port 5005
      :name "Attach to running Go process"
      :modes (go-mode go-ts-mode)
      :type "go"
      :request "attach"
      :mode "remote"
      :cwd (project-root (project-current))
      command "dlv"
      command-args ("connect" "127.0.0.1:5005")
      )
     ))

  :config
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; (global-set-key [f7] 'dape-step-in)
  ;; (global-set-key [f8] 'dape-next)
  ;; (global-set-key [f9] 'dape-continue)
  )


(provide 'init-dape)
