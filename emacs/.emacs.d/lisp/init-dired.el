;; init-dired.el --- Dired configurations.	-*- lexical-binding: t -*-

;; Commentry
;; `DIRctoryEDitor' configs

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode)
              ("_" . dired-create-empty-file))
  :config
  (setq dired-auto-revert-buffer #'dired-directory-changed-p)

  ;; Guess a default target directory
  (setq dired-dwim-target t)

  (setq dired-free-space 'first) ; Emacs 29.01
  (setq dired-make-directory-clickable t); Emacs 29.01
  (setq dired-mouse-drag-files t); Emacs 29.01

  (setq delete-by-moving-to-trash t)

  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  (add-hook 'dired-mode-hook #'hl-line-mode)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  ;; Show directory first
  (setq dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso")

  ;; Show git info in dired
  (use-package dired-git-info
    :bind (:map dired-mode-map
		        (")" . dired-git-info-mode)))

  ;; Allow rsync from dired buffers
  (use-package dired-rsync
    :bind (:map dired-mode-map
		        ("C-c C-r" . dired-rsync)))

  ;; Colorful dired
  (use-package diredfl
    :disabled
    :hook (dired-mode . diredfl-mode))

  ;; Shows icons
  (use-package nerd-icons-dired
    ;; :disabled
    :blackout
    ;; :if (featurep 'all-the-icons)
    :when (icons-displayable-p)
    :custom-face
    (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
    :hook (dired-mode . nerd-icons-dired-mode))

  ;; Extra Dired functionality
  (use-package dired-aux :ensure nil)
  (use-package dired-x
    :ensure nil
    :demand t
    :config
    (setq dired-guess-shell-alist-user
          `(("\\.pdf\\'" "zathura")
            ("\\.docx\\'" "libreoffice")
            ("\\.\\(?:djvu\\|eps\\)\\'" "zathura")
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "nsxiv")
            ("\\.\\(?:xcf\\)\\'" "gimp")
            ("\\.csv\\'" "libreoffice --calc")
            ("\\.tex\\'" "texmaker")
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" "mpv")
            ("\\.\\(?:mp3\\|flac\\)\\'" "mpv")
            ("\\.html?\\'" "firefox")))
    (setq dired-omit-files
          (concat dired-omit-files
                  "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))))

;; (use-package dired-hist
;;   :after dired
;;   :bind (:map dired-mode-map
;; 	          ("l" . dired-hist-go-back)
;; 	          ("r" . dired-hist-go-forward)))

;;<https://github.com/kickingvegas/casual-dired>
;; (use-package casual-dired
;;   :ensure nil
;;   :bind (:map dired-mode-map ("C-o" . 'casual-dired-tmenu)))

(use-package dired-preview
  :ensure t
  :config
  ;; Default values for demo purposes
  (setq dired-preview-delay 0.7)
  (setq dired-preview-max-size (expt 2 20))
  (setq dired-preview-ignored-extensions-regexp
	    (concat "\\."
		        "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
		        "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
		        "\\|.out\\|.o"
		        "\\|iso\\|epub\\|pdf\\)"))
  ;; (dired-preview-global-mode -1)
  )

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;; <https://github.com/Fuco1/dired-hacks>
(use-package dired-subtree
  :after dired
  :config
  (setq dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
	          ("<tab>" . dired-subtree-toggle)
	          ("<C-tab>" . dired-subtree-cycle)
	          ("<backtab>" . 'dired-subtree-remove)))

;; <https://github.com/mattiasb/dired-hide-dotfiles/blob/master/dired-hide-dotfiles.el>
;; <https://stackoverflow.com/questions/43628315/how-to-hide-one-dot-current-directory-in-dired-mode>

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-S-o" . dired-view-file-other-window))
  :config
  (defun dired-view-file-other-window ()
    "In Dired, examine a file in view mode on `other-window', returning to Dired when done.
When file is a directory, show it in this buffer if it is inserted.
Otherwise, display it in another buffer."
    (interactive nil dired-mode)
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
	      (or (and (cdr dired-subdir-alist)
		           (dired-goto-subdir file))
	          (dired file))
        (view-file-other-window file)))))


(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
