;;; init-db.el --- Database client configurations -*- lexical-binding: t -*-
;; Author: ukiran
;; Version: version
;; Package-Requires: mariadb

;;; Commentary:

;; Mariadb
(use-package sql
  :ensure nil
  :config
  (setq sql-mariadb-program "mariadb"))

(use-package sql-indent
  :ensure t)

(use-package sqlup-mode
  :ensure t
  :hook ((sql-mode . sqlup-mode)
         (sql-interactive-mode . sqlup-mode)))

(provide 'init-db)
;;; init-db.el ends here
