;;; init-funcs.el --- Personal Utility Functions -*- lexical-binding: t -*-

;;; Commentary:
;; Core custom helper functions for buffer manipulation, file management,
;; configuration reloading, and compilation utilities.

;;; Code:

;; File and buffer
(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer")))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (unless filename
      (user-error "No file is currently being edited"))
    (when (yes-or-no-p (format "Really delete '%s'?"
                               (file-name-nondirectory filename)))
      (delete-file filename)
      (kill-buffer (current-buffer))
      (message "Deleted and killed %s" filename))))

(defun rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((old-name (buffer-file-name)))
    (unless old-name
      (user-error "Buffer '%s' is not visiting a file!" (buffer-name)))
    (let* ((dir (file-name-directory old-name))
           (new-file-path (expand-file-name new-name dir)))
      (when (file-exists-p old-name)
        (rename-file old-name new-file-path 1))
      (set-visited-file-name new-file-path)
      (rename-buffer (file-name-nondirectory new-file-path)))))

(defun browse-this-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (user-error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (derived-mode-p 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (if filename
        (progn
          (kill-new filename)
          (message "Copied '%s'" filename))
      (warn "Current buffer is not attached to a file!"))))

(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

;; Reload configurations
(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))
(defalias 'ukiran-reload-init-file #'reload-init-file)

;; Open custom file
(defun find-custom-file ()
  "Open custom files."
  (interactive)
  (when (and (boundp 'custom-file) custom-file (file-exists-p custom-file))
    (find-file custom-file)))

;; Font & UI
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and (bound-and-true-p ukiran-icon)
       (or (featurep 'nerd-icons)
           (require 'nerd-icons nil t))))

(defun uk-toggle-time-bat-status ()
  "Toggle both `display-time-mode` and `display-battery-mode` together."
  (interactive)
  (if (and display-time-mode display-battery-mode)
      (progn
        (display-time-mode -1)
        (display-battery-mode -1))
    (progn
      (display-time-mode 1)
      (display-battery-mode 1))))

;; Compilation Helpers (Updated for straight.el paths)
(defun byte-compile-straight-build ()
  "Compile packages in straight build directory."
  (interactive)
  (let ((dir (expand-file-name "straight/build" user-emacs-directory)))
    (if (file-exists-p dir)
        (if (fboundp 'async-byte-recompile-directory)
            (async-byte-recompile-directory dir)
          (byte-recompile-directory dir 0 t))
      (message "Straight build directory not found."))))

(defun byte-compile-site-lisp ()
  "Compile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (file-exists-p dir)
        (if (fboundp 'async-byte-recompile-directory)
            (async-byte-recompile-directory dir)
          (byte-recompile-directory dir 0 t))
      (message "Site-lisp directory not found."))))

(defun native-compile-straight-build ()
  "Native-compile packages in straight build directory."
  (interactive)
  (let ((dir (expand-file-name "straight/build" user-emacs-directory)))
    (when (and (file-exists-p dir) (fboundp 'native-compile-async))
      (native-compile-async dir t))))

(defun native-compile-site-lisp ()
  "Native compile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (when (and (file-exists-p dir) (fboundp 'native-compile-async))
      (native-compile-async dir t))))

(provide 'init-funcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-funcs.el ends here
