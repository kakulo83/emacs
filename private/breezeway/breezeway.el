;;; package --- Summary
;;; Commentary:
;;;
;;; Code:

(setq sql-mysql-login-params nil)

(setq sql-connection-alist
			'((production-reader(sql-product 'mysql)
												 (sql-database (getenv "BREEZEWAY_PROD_CONNECTION_STR")))
			(staging-writer(sql-product 'mysql)
										 (sql-database (getenv "BREEZEWAY_STAGING_CONNECTION_STR")))
			(local (sql-product 'mysql)
						 (sql-database 'breezeway)
						 (sql-database (concat "mysql://"
																	 "breezeway"
																	 "127.0.0.1"
																	 ":3306"
																	 "breezeway")))))

(defun breezeway-db ()
	"Convenience function to connect to mysql."
	(interactive)
	(find-file "~/Developer/pybrzw/scratch.sql")
	(split-window-right)
	(call-interactively #'sql-connect))
