
(use-package orgmdb
    :ensure t
    :config
    (setq orgmdb-omdb-apikey "fd3c38ff")
    (setq orgmdb-poster-folder "~/Notes/Films/Posters")
    (setq orgmdb-fill-property-list
          '(poster country genre runtime director imdb-id imdb-rating)))

(provide 'init-utils)
