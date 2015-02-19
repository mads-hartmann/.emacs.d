(setq sql-connection-alist
      '((mudanashi (sql-product 'postgres)
                   (sql-port 5432)
                   (sql-server "localhost")
                   (sql-user "mudanashi")
                   (sql-database ""))))

(defun my-sql-connect (product connection)
  (require 'credentials)

  ;; update the password to the sql-connection-alist
  (let ((connection-info (assoc connection sql-connection-alist))
        (sql-password (car (last (assoc connection hartmann/postgres-credentials)))))
    (delete sql-password connection-info)
    (nconc connection-info `((sql-password ,sql-password)))
    (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
    (add-to-list 'sql-connection-alist connection-info))

  ;; connect to database
  (setq sql-product product)
  (sql-connect connection))

(defun postgres-mudanashi ()
  (interactive)
  (my-sql-connect 'postgres 'mudanashi))
