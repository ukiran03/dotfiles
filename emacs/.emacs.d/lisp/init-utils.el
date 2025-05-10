;;; init-utils.el --- summary -*- lexical-binding: t -*-


;;; Commentary:

;;; Code:

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package orgmdb
  :ensure t
  :config
  (setq orgmdb-omdb-apikey "fd3c38ff")
  (setq orgmdb-poster-folder "~/Notes/Films/Posters")
  (setq orgmdb-fill-property-list
        '(poster country genre runtime director imdb-id imdb-rating)))

;; #[clue.el:L13]
(use-package clue
  :load-path ("site-lisp/clue/")
  :config
  (setq clue-auto-enable-modes '(text-mode prog-mode markdown-mode)))

;;https://github.com/hlissner/dotfiles/blob/master/config/bspwm/bspwmrc#L31
;;bspc rule -a 'Emacs:emacs-everywhere' state=floating sticky=on
(use-package emacs-everywhere
  :config
  (setq emacs-everywhere-major-mode-function 'markdown-mode)
  :bind (:map emacs-everywhere-mode-map
              ("C-c C-c" . emacs-everywhere-finish)))

(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-define-keys))

;; (use-package kiwix
;;   :ensure t
;;   :after org
;;   :commands (kiwix-launch-server kiwix-at-point)
;;   ;; :bind (:map document-prefix ("w" . kiwix-at-point))
;;   :custom ((kiwix-server-type 'kiwix-serve-local)
;;            (kiwix-server-url "http://192.168.1.2:8000")
;;            (kiwix-server-port 8000)
;;            (kiwix-zim-dir (expand-file-name "/home/ukiran/zims")
;;                           ))
;;   :hook (org-load . org-kiwix-setup-link)
;;   :init (require 'org-kiwix)
;;   :config (add-hook 'org-load-hook #'org-kiwix-setup-link))

(use-package kiwix
  :ensure nil
  :commands (kiwix-launch-server kiwix-at-point)
  :config
  (setq kiwix-server-type 'kiwix-server-local)
  (setq kiwix-server-url "http://192.168.1.2:8000")
  (setq kiwix-server-port 8000)
  (setq kiwix-zim-dir (expand-file-name "/home/ukiran/zims")))

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
  :hook (after-init . ultra-scroll-mode))

(use-package stillness-mode
  ;;   :vc (:url "https://github.com/neeasade/stillness-mode.el")
  :after minibuffer
  :hook (after-init . stillness-mode))

(use-package keycast)



(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here

;; #[:meta:root:~/dotfiles/emacs/.emacs.d/site-lisp/clue/]
