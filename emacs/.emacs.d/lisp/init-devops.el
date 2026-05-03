;; init-devops.el -- Docker, K8s, etc. -*- lexical-binding: t -*-

(use-package docker
  :ensure t
  :demand t
  :hook ((docker-container-mode docker-image-mode) . hl-line-mode)
  :custom
  (docker-show-messages nil)
  :config
  (defun my/docker-format-in-use (container-count)
    "Format the CONTAINER-COUNT string into a usage indicator."
    (let ((count (string-trim container-count "[\" ]+")))
      (if (member count '("0" "null" ""))
          "-"
        "U")))

  (add-to-list 'docker-image-columns
               '(:name "Usage"
                       :width 7
                       :template "{{ json .Containers }}"
                       :sort nil
                       :format my/docker-format-in-use)
               t))

;; :bind ("C-c d" . docker) ; Global menu for containers, images,
(provide 'init-devops)
