;;; init-hugo.el --- Hugo Blog Post Automation Engine -*- lexical-binding: t -*-

;;; Commentary:
;; Scaffolds new Hugo blog posts in Org-mode with automated dates, titles, tags,
;; categories, and URL slugs.

;;; Code:

(defvar hugo-base-dir "~/Git/blog/"
  "Base directory for the Hugo blog.")

(defvar tag-list '("prog" "leetcode")
  "Default list of available tags for Hugo posts.")

(defvar categories-list '("codeblog" "personal")
  "Default list of available categories for Hugo posts.")

(defun today-is ()
  "Return current date formatted for post filenames."
  (format-time-string "%Y-%m-%d-"))

(defun now-is ()
  "Return current timestamp formatted for Hugo frontmatter."
  (concat (format-time-string "%Y-%m-%dT%T")
          ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
           (format-time-string "%z"))))

;;;###autoload
(defun uk-org-hugo-new-post ()
  "Creates a new org file in Hugo's content/posts directory."
  (interactive)
  (let* ((title (read-from-minibuffer "Title: "))
         (tags (completing-read "Tags: " tag-list))
         (categories (completing-read "Categories: " categories-list))
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
