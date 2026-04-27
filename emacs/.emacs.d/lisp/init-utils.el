;;; init-utils.el --- summary -*- lexical-binding: t -*-


;;; Commentary:

;;; Code:

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package blackout
  :config
  (dolist (mode '((lisp-interaction-mode . "λ")
                  (python-mode . "Py")
                  (emacs-lisp-mode . "Eλ")
                  (sh-mode . "SH")
                  (js-jsx-mode . "Js +JSX")
                  (js-mode . "Js")
                  (racket-mode . "RKT")
                  (scheme-mode . "λSCM")))
    (blackout (car mode) (cdr mode))))

;; yt-dlp
(use-package youtube-dl-emacs
  :vc (:url "https://github.com/skeeto/youtube-dl-emacs.git")
  :config
  (setq youtube-dl-arguments
        (append '("-f bestvideo[height<=1080]+bestaudio/best"
                  "--add-metadata"
                  "--write-subs")
                youtube-dl-arguments))
  (setq youtube-dl-directory "~/Videos"))

(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-define-keys))


;; (use-package leetcode
;;   :load-path ("site-lisp/leetcode")
;;   :config
;;   (setq leetcode-language "go")
;;   :commands (leetcode))  ; Add autoload for the main function if desired


;; Alternative UI's for Kubernetes
;; https://github.com/kubernetes-el/kubernetes-el
;; https://github.com/abrochard/kubel
;; https://github.com/jinnovation/kele.el

;;; https://github.com/eshelyaron/kubed
;;; https://eshelyaron.com/kubed.html
(use-package kubed
  :ensure t
  :bind (("H-a k" . kubed-prefix-map))
  :config
  ;; This enables the 'd' for delete, 'm' for mark workflow in lists
  (add-hook 'kubed-list-mode-hook #'hl-line-mode))

(use-package just-mode)

(use-package docker
  :ensure t
  ;; :bind ("C-c d" . docker) ; Global menu for containers, images,
  ;; volumes
  :config
  (setq docker-show-messages nil)
  ;; Makes the list buffers look clean and follow your cursor
  (add-hook 'docker-container-mode-hook #'hl-line-mode)
  (add-hook 'docker-image-mode-hook #'hl-line-mode))


;;; A better, more complete user interface for password-store
;;; https://github.com/rjekker/password-store-menu
(use-package password-store-menu
  :ensure t
  :init (password-store-menu-enable)
  :config
  (setq password-store-menu-key "C-c P"))

(use-package find-temp-file
  :bind ("H-f t" . find-temp-file)
  :config
  (setq find-temp-file-directory (concat temporary-file-directory "emacs_tmps/"))
  ;; Create the directory if it's missing
  (unless (file-exists-p find-temp-file-directory)
    (make-directory find-temp-file-directory t)))

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
