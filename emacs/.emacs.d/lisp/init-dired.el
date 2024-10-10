;; init-dired.el --- Dired configurations.	-*- lexical-binding: t -*-

;; Commentry
;; `DIRctoryEDitor' configs

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode))
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
    :ensure nil
    :disabled
    :hook (dired-mode . diredfl-mode))
  
  ;; Shows icons
  (use-package nerd-icons-dired
    :ensure nil
    :diminish
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
    (let ((cmd "xdg-open"))
      (setq dired-guess-shell-alist-user
            `(("\\.pdf\\'" ,cmd)
              ("\\.docx\\'" ,cmd)
              ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
              ("\\.\\(?:xcf\\)\\'" ,cmd)
              ("\\.csv\\'" ,cmd)
              ("\\.tex\\'" ,cmd)
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
              ("\\.html?\\'" ,cmd)
              ("\\.md\\'" ,cmd))))

    (setq dired-omit-files
          (concat dired-omit-files
                  "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))))

(use-package dired-hist
  :after dired
  :bind (:map dired-mode-map
	      ("l" . dired-hist-go-back)
	      ("r" . dired-hist-go-forward)))

;;<https://github.com/kickingvegas/casual-dired>
(use-package casual-dired
  :ensure nil
  :bind (:map dired-mode-map ("C-o" . 'casual-dired-tmenu)))

(use-package dired-preview
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
  ;; Enable `dired-preview-mode' in a given Dired buffer or do it
  ;; globally:
  (dired-preview-global-mode -1))

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
(use-package dired-collapse)
(use-package dired-open
  :after dired
  :config
  (setq dired-open-extensions '(("gif" . "mpv")
                                ("jpg" . "ristretto")
                                ("webp" . "ristretto")
                                ("png" . "ristretto")
                                ("pdf" . "zathura")
                                ("mkv" . "mpv")
                                ("webm" . "mpv")
                                ("mp4" . "mpv"))))
;; <https://github.com/mattiasb/dired-hide-dotfiles/blob/master/dired-hide-dotfiles.el>
;; <https://stackoverflow.com/questions/43628315/how-to-hide-one-dot-current-directory-in-dired-mode>

(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here

