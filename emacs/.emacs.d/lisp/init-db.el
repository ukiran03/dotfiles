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

;;; Code:

(message "Hello, World")

(provide 'init-db)
;;; init-db.el ends here
