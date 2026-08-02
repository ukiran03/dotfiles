;;; init-mail.el --- Mail configurations. -*- lexical-binding: t -*-

;;; Commentary:
;; Configures Notmuch mail client settings and saved searches.

;;; Code:

(use-package notmuch
  :straight t
  :commands (notmuch notmuch-mua-new-mail)
  :config
  (setq notmuch-show-logo nil
        notmuch-search-oldest-first nil
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches
                                 notmuch-hello-insert-recent-searches)
        notmuch-saved-searches
        `(( :name "Inbox"
            :query "tag:inbox"
            :sort-order newest-first
            :key ,(kbd "i"))
          ( :name "All Unread (inbox)"
            :query "tag:unread and tag:inbox"
            :sort-order newest-first
            :key ,(kbd "u"))
          ( :name "flagged"
            :query "tag:flagged"
            :key "f")
          ( :name "sent"
            :query "tag:sent"
            :sort-order newest-first
            :key "t")
          ( :name "drafts"
            :query "tag:draft"
            :sort-order newest-first
            :key "d")
          ( :name "reddiushakiran@"
            :query "to:reddiushakiran@gmail.com"
            :sort-order newest-first)
          ( :name "ushakiranreddi@"
            :query "to:ushakiranreddi@gmail.com"
            :sort-order newest-first))))

(provide 'init-mail)
;;; init-mail.el ends here
