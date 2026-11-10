;;; init-db.el --- Database client configurations -*- lexical-binding: t -*-

;;; Commentary:
;; Structural setup for SQL file editing and interactive MariaDB clients.

;;; Code:

;; Built-in SQL client settings
(use-package sql
  :straight (:type built-in)
  :custom
  (sql-mariadb-program "mariadb")
  ;; Optional bonus: Define a safe, fast preset to connect to your local dev DB
  ;; You can launch this instantly using `M-x sql-connect`
  (sql-connection-alist
   '((dev-db (sql-product 'mariadb)
             (sql-user "root")
             (sql-database "development")
             (sql-server "localhost")
             (sql-port 3306)))))

;; Smart SQL Syntax Indentation Engine
(use-package sql-indent
  :straight t
  :hook (sql-mode . sqlind-minor-mode))

(use-package sqlformat
  :straight t
  :custom
  (sqlformat-command 'sqlformat)) ; pgformatter etc.

;; Automatic Keyword Uppercasing Engine
(use-package sqlup-mode
  :straight t
  :hook ((sql-mode sql-interactive-mode) . sqlup-mode)
  :config
  ;; Do not automatically uppercase the column or variable token "name"
  (add-to-list 'sqlup-blacklist "name"))

(provide 'init-db)
;;; init-db.el ends here
