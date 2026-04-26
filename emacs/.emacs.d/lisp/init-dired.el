;; init-dired.el --- Dired configurations.	-*- lexical-binding: t -*-

;; Commentry
;; `DIRctoryEDitor' configs
(use-package emacs
  :ensure nil
  :bind ("H-j" . dired-jump))

(use-package dired
  :ensure nil
  :hook ((dired-mode . dired-omit-mode)
         (dired-mode . hl-line-mode)
         (dired-mode . dired-hide-details-mode))
  :bind (:map dired-mode-map
              ("C-c C-p" . wdired-change-to-wdired-mode)
              ("_"       . dired-create-empty-file)
              ("."       . dired-omit-mode))
  :config
  (setq dired-auto-revert-buffer #'dired-directory-changed-p
        dired-dwim-target t
        dired-free-space 'first            ; Emacs 29+
        dired-make-directory-clickable t   ; Emacs 29+
        dired-mouse-drag-files t           ; Emacs 29+
        delete-by-moving-to-trash t
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso"))

;; Standard Extensions (dired-x & dired-aux)
(use-package dired-x
  :ensure nil
  :after dired
  :config
  (setq dired-omit-files
        (rx (or (seq line-end ".elc")          ; Ends with .elc
                (seq line-start (opt ".") "#") ; Starts with # or .#
                (seq line-start "."))))         ; Starts with . (dot)
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

(use-package dired-gitignore
  :ensure t
  :after dired
  :bind (:map dired-mode-map ("h" . dired-gitignore-mode)))

;; Third-Party Packages (Keep these separate)
(use-package dired-git-info
  :after dired
  :bind (:map dired-mode-map (")" . dired-git-info-mode)))

(use-package dired-rsync
  :after dired
  :bind (:map dired-mode-map ("C-c C-r" . dired-rsync)))

(use-package diredfl
  :disabled
  :hook (dired-mode . diredfl-mode))

;; (use-package dired
;;   :ensure nil
;;   :hook (dired-mode . dired-omit-mode)
;;   :bind (:map dired-mode-map
;;               ( "."     . dired-omit-mode))
;;   :custom (dired-omit-files (rx (seq bol "."))))

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
  :disabled
  :ensure t
  :after dired
  :config
  ;; Default values for demo purposes
  (setq dired-preview-delay 0.7)
  (setq dired-preview-max-size (expt 2 20))
  (setq dired-preview-ignored-extensions-regexp
        (rx "." (or "mkv" "webm" "mp4" "mp3" "ogg" "m4a"    ;; Media
                    "gz" "zst" "tar" "xz" "rar" "zip" "iso" ;; Archives
                    "epub" "pdf" ;; Documents
                    "out" "o")   ;; Build Artifacts
            line-end))
  (dired-preview-global-mode 1))

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
