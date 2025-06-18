;; https://github.com/isamert/orgmdb.el
;; https://isamert.net/2021/05/09/migrating-my-imdb-ratings-list-and-watch-list-into-org-mode.html

(defun my/dired-du ()
  "Run 'du -hc' on the directory under the cursor in Dired."
  (interactive)
  (let ((current-dir (dired-get-file-for-visit)))        ; some-comment
    (if (file-directory-p current-dir)
        (dired-do-async-shell-command "du -hc" nil (list current-dir))
      (message "The current point is not a directory"))))


;;;; Packages:

;; Request.el -- Easy HTTP request for Emacs Lisp
;; Doc: <https://tkf.github.io/emacs-request/>
;; <https://github.com/tkf/emacs-request>

;; A Growl-like alerts notifier for Emacs
;; <https://github.com/jwiegley/alert>

;; View Large Files in Emacs
;; <https://github.com/m00natic/vlfi>

;; HTTP headers, media-types, methods, relations and status codes, all
;; summarized and linking to their specification.
;; <https://github.com/for-GET/know-your-http-well>
;; Resources:
;; <https://github.com/dret/webconcepts> *
;; <https://twitter.com/DanaDanger/status/183316183494311936>
;; <https://x.com/stevelosh/status/372740571749572610?lang=en>

;; superfast navigation and remote control for Emacs source code
;; buffers (based on Emacs occur-mode)
;; <https://github.com/alphapapa/navi>

