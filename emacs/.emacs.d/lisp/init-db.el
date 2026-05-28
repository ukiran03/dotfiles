;;; init-db.el --- Database client configurations -*- lexical-binding: t -*-

;;; Commentary:
;; Structural setup for SQL file editing and interactive MariaDB clients.

;;; Code:

;; Built-in SQL client settings
(use-package sql
  :ensure nil
  :config
  (setq sql-mariadb-program "mariadb")

  ;; Optional bonus: Define a safe, fast preset to connect to your local dev DB
  ;; You can launch this instantly using `M-x sql-connect`
  (setq sql-connection-alist
        '((dev-db (sql-product 'mariadb)
                  (sql-user "root")
                  (sql-database "development")
                  (sql-server "localhost")
                  (sql-port 3306)))))

;; Smart SQL Syntax Indentation Engine
(use-package sql-indent
  :ensure t
  :blackout
  :hook(sql-mode . sqlind-minor-mode))

;; Automatic Keyword Uppercasing Engine
(use-package sqlup-mode
  :ensure t
  :blackout
  :hook ((sql-mode sql-interactive-mode) . sqlup-mode)
  :config
  ;; Do not automatically uppercase the column or variable token "name"
  (add-to-list 'sqlup-blacklist "name"))

(provide 'init-db)
;;; init-db.el ends here
