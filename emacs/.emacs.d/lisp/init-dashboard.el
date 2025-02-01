;; init-dashboard.el --- Initialize dashboard configurations.	-*- lexical-binding: t -*-

;;; Commentry
;;
;; Dashboard configuration


(eval-when-compile
  (require 'init-funcs))
  ;; (require 'init-custom))


(use-package dashboard
  :diminish dashboard-mode
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
  (setq dashboard-banner-logo-title "It's always You vs You"
        dashboard-banners-directory (expand-file-name "avatars/" user-emacs-directory)
	    dashboard-startup-banner (concat dashboard-banners-directory "musashi1.jpg")

	    dashboard-projects-backend 'project-el
	    dashboard-path-style 'truncate-middle
	    dashboard-path-max-length 60
	    dashboard-center-content t
        dashboard-vertically-center-content nil ;;@
        dashboard-remove-missing-entry t
	    dashboard-show-shortcuts t
	    dashboard-items '((recents . 6)
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


(provide 'init-dashboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dashboard.el ends here
