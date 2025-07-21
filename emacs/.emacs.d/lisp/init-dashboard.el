;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-

;;; Commentry
;;
;; Dashboard configuration


(eval-when-compile
  (require 'init-funcs))
;; (require 'init-custom))


(use-package dashboard
  :custom-face
  (dashboard-heading ((t (:weight bold))))
  (dashboard-items-face ((t (:weight normal))))
  (dashboard-no-items-face ((t (:weight normal))))
  :hook (dashboard-mode . (lambda ()
			                ;; No title
			                (setq-local frame-title-format "GNU Emacs")
			                ;; Enable `page-break-lines-mode'
			                (when (fboundp 'page-break-lines-mode)
			                  (page-break-lines-mode 1))))
  :init
  (setq dashboard-banner-logo-title "I am not this body"
        dashboard-banners-directory (expand-file-name "avatars/" user-emacs-directory)
	    dashboard-startup-banner (concat dashboard-banners-directory "musashi1.jpg")
        ;; dashboard-startup-banner 'logo

	    dashboard-projects-backend 'project-el
	    dashboard-path-style 'truncate-middle
	    dashboard-path-max-length 60
	    dashboard-center-content t
        dashboard-vertically-center-content nil ;;@
        dashboard-remove-missing-entry t
	    dashboard-show-shortcuts t
	    dashboard-items '(
                          ;; (recents . 6)
                          ;; (bookmarks . 5)
                          ;; (registers . 3)
                          ;; (agenda . 3)
		                  (projects . 6))
	    dashboard-bookmarks-show-base t
	    dashboard-bookmarks-item-format "%s"
	    dashboard-set-file-icons t
	    dashboard-set-heading-icons t

        dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline
                                    dashboard-insert-footer))

  (setq  dashboard-footer-messages
         '("Do your duty without attachment to the results."
	       "Change is the law of the universe."
	       "Set your heart upon your work but never its reward."
	       "The mind is restless and difficult to restrain, but it is subdued by practice."
	       "The soul is neither born nor does it die; it is eternal, indestructible, and timeless."
	       "He who has no attachments can really love others, for his love is pure and divine."
	       "One who sees inaction in action and action in inaction is intelligent among men."
	       "When meditation is mastered, the mind is unwavering like the flame of a lamp in a windless place."
	       "Man is made by his belief. As he believes, so he is.")

  	     dashboard-footer-icon
	     (if (icons-displayable-p)
	         (nerd-icons-mdicon "nf-md-om" :face 'nerd-icons-lred)
	       (propertize ">" 'face 'dashboard-footer)))
  (dashboard-setup-startup-hook))

;; (use-package welcome-dashboard
;;   :vc (:url "https://github.com/konrad1977/welcome-dashboard")
;;   ;; :ensure nil ;; when using local file and not straight nor use-package
;;   :config
;;   (setq welcome-dashboard-latitude 56.7365
;;         welcome-dashboard-longitude 16.2981     ;; latitude and longitude must be set to show weather information
;;         welcome-dashboard-use-nerd-icons t      ;; Use nerd icons instead of all-the-icons
;;         welcome-dashboard-path-max-length 75
;;         welcome-dashboard-show-file-path t      ;; Hide or show filepath
;;         welcome-dashboard-use-fahrenheit nil    ;; show in celcius or fahrenheit.
;;         welcome-dashboard-min-left-padding 10
;;         welcome-dashboard-image-file (expand-file-name "avatars/musashi1.jpg" user-emacs-directory)
;;         welcome-dashboard-image-width 200
;;         welcome-dashboard-image-height 169
;;         welcome-dashboard-max-number-of-todos 5
;;         welcome-dashboard-title (concat "Welcome " user-full-name))
;;   (welcome-dashboard-create-welcome-hook))

(provide 'init-dashboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dashboard.el ends here
