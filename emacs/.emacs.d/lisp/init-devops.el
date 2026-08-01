;; init-devops.el -- Docker, K8s, etc. -*- lexical-binding: t -*-

(use-package docker
  :straight t
  ;; :demand t
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


;; Alternative UI's for Kubernetes
;; https://github.com/kubernetes-el/kubernetes-el
;; https://github.com/abrochard/kubel
;; https://github.com/jinnovation/kele.el

;;; https://github.com/eshelyaron/kubed
;;; https://eshelyaron.com/kubed.html
(use-package kubed
  :straight t
  :bind (("H-a k" . kubed-prefix-map))
  :config
  ;; This enables the 'd' for delete, 'm' for mark workflow in lists
  (add-hook 'kubed-list-mode-hook #'hl-line-mode))


(provide 'init-devops)
