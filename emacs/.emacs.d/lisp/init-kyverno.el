
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;; 1. Add to Emacs' internal search list
(add-to-list 'exec-path "/home/ukiran/.local/share/pnpm")

;; 2. Update the environment PATH so sub-processes (like devcontainer-cli) find it
(setenv "PATH" (concat "/home/ukiran/.local/share/pnpm:" (getenv "PATH")))

(use-package devcontainer
  :ensure t
  ;; :vc (:fetcher github :repo "johannes-mueller/devcontainer-mode")
  :vc (:url "https://github.com/johannes-mueller/devcontainer-mode.git"
            :rev :newest)
  :init
  (setq devcontainer-engine 'docker)
  :config
  ;; This tells Emacs to run 'make' inside the container when you are
  ;; in a devcontainer buffer
  (devcontainer-mode 1))

(use-package tramp
  :config
  ;; Crucial: This ensures TRAMP uses the container's own PATH (so it
  ;; finds go, gopls, etc.)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package eglot
  :ensure t
  :config
  ;; Link go-mode to the gopls binary inside the container
  (add-to-list 'eglot-server-programs '((go-mode go-ts-mode) . ("gopls"))))

(provide 'init-kyverno)
