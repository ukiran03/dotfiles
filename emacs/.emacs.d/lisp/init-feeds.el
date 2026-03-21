
;; <https://github.com/skeeto/elfeed>
(use-package elfeed
  :bind (:map elfeed-show-mode-map
              ("V" . 'my-elfeed-mpv-play))
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-show-entry-switch 'display-buffer
        elfeed-feeds nil
        elfeed-search-filter "@6-months-ago +unread -fun")

  (defun my-elfeed-mpv-play ()
    "Play the current elfeed entry with mpv."
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link (start-process "elfeed-mpv" nil "mpv" link)))))

;; <https://github.com/remyhonig/elfeed-org>
;; Configure Elfeed with org mode
(use-package elfeed-org
  :ensure t
  :init
  (setq rmh-elfeed-org-files
        (list (expand-file-name "feeds.org" org-directory)))
  :config
  (elfeed-org))

;; <https://github.com/karthink/elfeed-tube>
(use-package elfeed-tube :ensure nil)

;; <https://github.com/sp1ff/elfeed-score>
(use-package elfeed-score :ensure nil)

;; <https://github.com/Manoj321/elfeed-dashboard>
(use-package elfeed-dashboard :ensure nil)

(provide 'init-feeds)
