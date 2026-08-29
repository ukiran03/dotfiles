;;; init-hugo.el --- Hugo Blog Post Automation Workflow -*- lexical-binding: t -*-

;;; Commentary:
;; Scaffolds new Hugo blog posts in Org-mode with automated dates, titles,
;; tags, categories, and URL slugs using `ox-hugo'.

;;; Code:

(defun org-hugo-export-to-md-and-open ()
  "Export the current Org buffer to Markdown and open the resulting file."
  (interactive)
  (org-open-file (org-hugo-export-to-md)))

(defvar-keymap my/hugo-command-map
  :doc "Keymap for ox-hugo export commands."
  :name "ox-hugo Export"
  "h" '("Export to MD file" . org-hugo-export-to-md)
  "o" '("Export to MD & Open" . org-hugo-export-to-md-and-open)
  "t" '("Temporary MD Buffer" . org-hugo-export-as-md))

(use-package ox-hugo
  :straight t
  :after ox
  :demand t)

(defcustom ox-hugo-base-dir "~/Git/blog/"
  "Base directory for Hugo project."
  :type 'directory
  :group 'ox-hugo)

(defvar ox-hugo-tag-list nil
  "List of available tags for completion.")

(defvar ox-hugo-category-list '("Personal" "Codeblog" "Notes")
  "List of available categories for completion.")

;;;###autoload
(defun uk-org-hugo-new-post ()
  "Create a new Org file for ox-hugo in the 'org-content/' directory."
  (interactive)
  (let* ((title (read-string "Title: "))
         ;; Downcase first so regex correctly catches uppercase letters
         (slug (downcase (replace-regexp-in-string "[^a-z0-9]+" "-" title)))
         (clean-slug (replace-regexp-in-string "^-\\|-$" "" slug))

         ;; Use completing-read-multiple if you want comma-separated tags/categories,
         ;; or stick to standard completing-read if single-item is intended.
         (tags (completing-read "Tags: " ox-hugo-tag-list))
         (categories (completing-read "Category: " ox-hugo-category-list))

         (default-filename (concat clean-slug ".org"))
         (filename (read-string "Filename: " default-filename))

         (date-prefix (format-time-string "%Y-%m-%d-"))
         (blogfile (concat date-prefix filename))
         (bundle (file-name-sans-extension (file-name-nondirectory blogfile)))
         (content-dir (expand-file-name "org-content/" ox-hugo-base-dir))
         (path (expand-file-name blogfile content-dir)))

    (if (file-exists-p path)
        (message "File already exists: %s" path)
      ;; Open/create the file directly
      (progn
        (find-file path)
        (org-mode)
        ;; Insert standard Hugo / Org headers
        (insert (format "#+TITLE: %s\n" title)
                (format "#+DATE: %s\n" (format-time-string "%Y-%m-%dT%T%:z"))
                (format "#+HUGO_TYPE: posts\n")
                (format "#+HUGO_CATEGORIES: %s\n" categories)
                (format "#+HUGO_TAGS: %s\n" tags)
                (format "#+HUGO_BUNDLE: %s\n" bundle)
                "#+EXPORT_FILE_NAME: index\n"
                "#+DRAFT: true\n"
                "#+DESCRIPTION: ")
        ;; Save the newly created file immediately
        (save-buffer)
        (goto-char (point-max))
        (message "Created new post at: %s" path)))))

(provide 'init-hugo)
;;; init-hugo.el ends here
