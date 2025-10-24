;;; init-hugo.el --- summary -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq hugo-base-dir "~/.tmp/hugo/quickstart/")
(setq hugo-base-dir "~/Git/blog/")
(setq tag-list '("prog" "leetcode"))
(setq categories-list '("codeblog" "personal"))
(defun today-is ()
  (format-time-string "%Y-%m-%d-"))

(defun now-is ()
  (concat (format-time-string "%Y-%m-%dT%T")
          ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
           (format-time-string "%z"))))

(defun uk-org-hugo-new-post ()
  "Creates a new org file in Hugo's content/posts directory"
  (interactive)
  (let* ((title (read-from-minibuffer "Title: "))
         (tags (completing-read "Tags: " tag-list))
         (categories (completing-read "Tags: " categories-list))
         (filename
          (read-from-minibuffer
           "Filename: "
		   (replace-regexp-in-string
            "-\\.org" ".org"
			(concat (downcase
					 (replace-regexp-in-string "[^a-z0-9]+" "-" title))
					".org"))))
	     (blogfile (concat "posts/" (today-is) filename))
	     (url (concat "posts/"
                      (today-is)
                      (downcase
		               (replace-regexp-in-string "[^a-z0-9]+" "-" title))))
         (path (concat hugo-base-dir "content/" blogfile)))

    (if (file-exists-p path)
        (message "File already exists!")
      (message blogfile)
      (switch-to-buffer (generate-new-buffer blogfile))
      (insert "#+TITLE: " title)
      (insert "\n#+DATE: " (now-is))
      (insert "\n#+TYPE: posts")
      (insert "\n#+URL: /" url "/")
      (insert "\n#+CATEGORIES: " categories)
      (insert "\n#+TAGS[]: " tags)
      (insert "\n#+DRAFT: true\n\n")
      (org-mode)
      (write-file path)
      (goto-char (point-max)))))

(provide 'init-hugo)
;;; init-hugo.el ends here
