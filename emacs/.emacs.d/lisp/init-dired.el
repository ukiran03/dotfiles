;;; init-dired.el --- Dired configurations -*- lexical-binding: t -*-

;;; Commentary:
;; A fast, unified, and interactive keyboard-driven filesystem layout manager.

;;; Code:

;; 1. Global Jump Binding
(use-package emacs
  :straight nil
  :bind ("H-j" . dired-jump))

;; 2. Consolidated Core Dired Configuration
(use-package dired
  :straight nil
  :hook ((dired-mode . dired-omit-mode)
         (dired-mode . hl-line-mode)
         (dired-mode . dired-hide-details-mode))
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode)
              ("_"       . dired-create-empty-file)
              ("."       . dired-omit-mode)
              ("C-S-o"   . dired-view-file-other-window))
  :config
  ;; Switch Layout Mechanics
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")

  ;; Realtime UX Update Controls
  (setq dired-auto-revert-buffer #'dired-directory-changed-p
        dired-dwim-target t ; Intelligently targets adjacent splits for operations
        dired-free-space 'first
        dired-make-directory-clickable t
        dired-mouse-drag-files t)

  ;; Trash/Deletions Policy Security
  (setq delete-by-moving-to-trash t
        dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  ;; Embedded custom helper inside config
  (defun dired-view-file-other-window ()
    "In Dired, examine a file in view mode on `other-window', returning to
Dired when done.  When file is a directory, show it in this buffer if it
is inserted.  Otherwise, display it in another buffer."
    (interactive nil dired-mode)
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (or (and (cdr dired-subdir-alist)
                   (dired-goto-subdir file))
              (dired file))
        (view-file-other-window file)))))

;; 3. Core Standard Extension Adjustments
(use-package dired-x
  :straight nil
  :after dired
  :config
  ;; File Omit Regex
  (setq dired-omit-files
        (rx (or (seq line-end ".elc")
                (seq line-start (opt ".") "#")
                (seq line-start "."))))

  ;; External App Associations Engine
  (setq dired-guess-shell-alist-user
        `((,(rx ".pdf" eos) "zathura")
          (,(rx ".docx" eos) "libreoffice")
          (,(rx "." (or "djvu" "eps") eos) "zathura")
          (,(rx "." (or "jpg" "jpeg" "png" "gif" "xpm") eos) "nsxiv")
          (,(rx ".xcf" eos) "gimp")
          (,(rx ".csv" eos) "libreoffice --calc")
          (,(rx "." (or "mp4" "mkv" "avi" "flv" "rm" "rmvb" "ogv")
                (opt ".part") eos) "mpv")
          (,(rx "." (or "mp3" "flac") eos) "mpv")
          (,(rx ".htm" (opt "l") eos) "firefox"))))

;; 4. Third-Party Extension Layers
(use-package dired-gitignore
  :straight t
  :after dired
  :bind (:map dired-mode-map ("h" . dired-gitignore-mode)))

(use-package dired-git-info
  :straight t
  :after dired
  :bind (:map dired-mode-map (")" . dired-git-info-mode)))

(use-package dired-rsync
  :straight t
  :after dired
  :bind (:map dired-mode-map ("C-c C-r" . dired-rsync)))

(use-package dired-subtree
  :straight t
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)
              ("<backtab>" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

;; 5. Mini-Buffer Quick Path Directory Traversal
(use-package consult-dir
  :straight t
  :bind ("C-x C-d" . consult-dir)
  :config
  ;; Safe lazy-loaded configuration injection for Vertico userspace
  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "C-x C-d") #'consult-dir)
    (define-key vertico-map (kbd "C-x C-j") #'consult-dir-jump-file)))

(use-package dired-sidebar
  :straight t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :custom
  (dired-sidebar-window-fixed nil)
  (dired-sidebar-width 25)
  (dired-sidebar-theme 'none)
  :bind
  ([f8] . dired-sidebar-toggle-with-current-directory))


(provide 'init-dired)
;;; init-dired.el ends here
