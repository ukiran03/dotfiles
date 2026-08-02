;;; init-feeds.el --- Elfeed RSS Dashboard Engine -*- lexical-binding: t -*-

;;; Commentary:
;; A unified keyboard-driven feed aggregator supporting automatic video
;; playback via MPV, Org-mode subscription lists, scoring layouts, and rich
;; text styling.

;;; Code:

;; Global Sync/Startup Command Shorthand
;;;###autoload
(defun uk-elfeed (arg)
  "Open `elfeed`. With a prefix ARG (\\[universal-argument]), initialize
`elfeed-org` and force a full async database fetch before opening."
  (interactive "P")
  (when arg
    (require 'elfeed-org)
    (elfeed-org)
    (elfeed-update))
  (elfeed))

;; Core Extensible Database and Engine Setup
(use-package elfeed
  :straight t
  :bind (:map elfeed-show-mode-map
              ("V" . my/elfeed-mpv-play))
  :hook (elfeed-show-mode . visual-line-mode)

  :preface
  ;; Declare helper functions early so that variables can safely reference them
  (defun elfeed-display-buffer (buf &optional _act)
    "Custom split window behavior configuration showcasing entry views at
half frame bounds."
    (pop-to-buffer buf)
    (set-window-text-height (get-buffer-window) (round (* 0.5 (frame-height)))))

  (defun my/elfeed-mpv-play ()
    "Asynchronously launch an isolated MPV client instance targeting the
current article URL."
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (if link
          (progn
            (start-process "elfeed-mpv" nil "mpv" link)
            (message "Streaming target media via MPV..."))
        (message "Warning: No valid structural URL target found for this entry."))))

  :config
  ;; Global Properties and View Engine Rules
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-feeds nil ; Overridden by elfeed-org files below
        elfeed-search-filter "@6-months-ago +unread -trash -fun -hn")

  ;; Safely map custom function now that it's compile-declared in :preface
  (setq elfeed-show-entry-switch #'elfeed-display-buffer))

;; Org-Mode Subscriber File Layout Association
(use-package elfeed-org
  :straight t
  :after elfeed
  :init
  (setq rmh-elfeed-org-files (list (expand-file-name "feeds.org" org-directory)))
  :config
  (elfeed-org))

;; Advanced Ecosystem Tool Integrations
;; <https://github.com/karthink/elfeed-tube>
;; <https://github.com/sp1ff/elfeed-score>
;; <https://github.com/Manoj321/elfeed-dashboard>

(use-package elfeed-tube
  :disabled
  :straight t
  :after elfeed
  :config
  ;; Automatically hooks into elfeed to fetch metadata, thumbnails, and
  ;; transcripts from YouTube links
  (elfeed-tube-setup))

(use-package elfeed-score
  :disabled
  :straight t
  :after elfeed
  :config
  ;; Activates interactive engine layout parameters to rank down noise rules
  ;; automatically
  (elfeed-score-enable))

(use-package elfeed-dashboard
  :disabled
  :straight t
  :after elfeed
  :config
  (setq elfeed-dashboard-file
        (expand-file-name "elfeed-dashboard.org" user-emacs-directory)))

(provide 'init-feeds)
;;; init-feeds.el ends here
