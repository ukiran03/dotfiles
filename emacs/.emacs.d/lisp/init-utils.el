
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

(provide 'init-utils)
;; #[:meta:root:~/dotfiles/emacs/.emacs.d/site-lisp/clue/]
