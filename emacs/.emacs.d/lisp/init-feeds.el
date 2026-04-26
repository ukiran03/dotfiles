
;; <https://github.com/skeeto/elfeed>
(use-package elfeed
  :bind (:map elfeed-show-mode-map
              ("V" . 'my-elfeed-mpv-play))
  :hook ((elfeed-show-mode . visual-line-mode)
         ;; (elfeed-show-mode . olivetti-mode)
         )
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-show-entry-switch 'display-buffer
        elfeed-feeds nil
        elfeed-search-filter "@6-months-ago +unread -trash -fun -hn"
        ;; elfeed-show-entry-switch #'elfeed-display-buffer ;; Not working
        )

  (defun my-elfeed-mpv-play ()
    "Play the current elfeed entry with mpv."
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link (start-process "elfeed-mpv" nil "mpv" link))))

  (defun elfeed-display-buffer (buf &optional act)
    (pop-to-buffer buf)
    (set-window-text-height (get-buffer-window) (round (* 0.5 (frame-height)))))

  (defun uk-elfeed-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load) (elfeed) (elfeed-search-update--force)))

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
