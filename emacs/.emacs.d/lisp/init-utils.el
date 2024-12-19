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


(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here

;; #[:meta:root:~/dotfiles/emacs/.emacs.d/site-lisp/clue/]
