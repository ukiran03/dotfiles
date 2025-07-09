(use-package notmuch
  :commands (notmuch notmuch-mua-new-mail))

(use-package notmuch
  :defer t
  :config
  (setq notmuch-show-logo nil
        ;; notmuch-column-control 1.0
        ;; notmuch-hello-auto-refresh t
        ;; notmuch-hello-recent-searches-max 20
        ;; notmuch-hello-thousands-separator ""
        ;; notmuch-hello-sections '(notmuch-hello-insert-saved-searches)
        ;; notmuch-show-all-tags-list t
        )
  (setq notmuch-hello-sections '(
                                 ;; notmuch-hello-insert-header
                                 notmuch-hello-insert-saved-searches
                                 ;; notmuch-hello-insert-search
                                 notmuch-hello-insert-recent-searches
                                 ;; notmuch-hello-insert-alltags
                                 ;; notmuch-hello-insert-footer
                                 )))

(use-package notmuch
  :config
  (setq notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i")
                                 (:name "unread" :query "tag:unread" :key "u")
                                 (:name "flagged" :query "tag:flagged" :key "f")
                                 (:name "sent" :query "tag:sent" :key "t")
                                 (:name "drafts" :query "tag:draft" :key "d")
                                 (:name "all mail" :query "*" :key "a")
                                 (:name "ushakiranreddi@" :query "to:ushakiranreddi@gmail.com")
                                 (:name "reddiushakiran@" :query "to:reddiushakiran@gmail.com"))))

(provide 'init-mail)
