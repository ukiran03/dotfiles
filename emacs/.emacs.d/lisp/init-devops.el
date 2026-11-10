;;; init-devops.el -- Docker, K8s, etc. -*- lexical-binding: t -*-

;;; Commentary:
;; DevOps integration including Docker and Kubernetes client setups.

;;; Code:

(use-package docker
  :straight t
  :preface
  (defun my/docker-format-in-use (container-count)
    "Format the CONTAINER-COUNT string into a usage indicator."
    (let ((count (string-trim container-count "[\" ]+")))
      (if (member count '("0" "null" ""))
          "-"
        "U")))

  :custom
  (docker-show-messages nil)

  :hook
  ((docker-container-mode docker-image-mode) . hl-line-mode)

  :config
  (add-to-list 'docker-image-columns
               '(:name "Usage"
                       :width 7
                       :template "{{ json .Containers }}"
                       :sort nil
                       :format my/docker-format-in-use)
               t))

;; Global menu for containers, images, etc.
;; (global-set-key (kbd "C-c d") #'docker)


;; Alternative UI's for Kubernetes
;; https://github.com/kubernetes-el/kubernetes-el
;; https://github.com/abrochard/kubel
;; https://github.com/jinnovation/kele.el

;;; https://github.com/eshelyaron/kubed
;;; https://eshelyaron.com/kubed.html
(use-package kubed
  :straight t
  :bind
  (("H-a k" . kubed-prefix-map))
  :hook
  ;; This enables the 'd' for delete, 'm' for mark workflow in lists
  (kubed-list-mode . hl-line-mode))

(provide 'init-devops)
;;; init-devops.el ends here
