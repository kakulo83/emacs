;;; package --- Summary
;;; Commentary:
;;;
;;; Code:
(defun fountain()
	"Run the fountain web app, includes self-serve and monolith."
	(interactive)
	(run-monolith)
	(run-ms-self-serve))

(defun run-monolith ()
	"Run the monolith application."
	(let ((path-to-monolith "cd ~/Developer/monolith\n"))
		(multi-vterm)
		(rename-buffer "monolith rails server")

		(process-send-string nil path-to-monolith)
		(process-send-string nil "rvm use $(cat .ruby-version)\n")
		(process-send-string nil "rvm gemset use $(cat .ruby-gemset)\n")
		(process-send-string nil "bundle exec rails server\n")

		(multi-vterm)
		(rename-buffer "monolith webpack")
		(process-send-string nil path-to-monolith)
		(process-send-string nil "yarn run webpack:development\n")

		(multi-vterm)
		(rename-buffer "monolith sidekiq")
		(process-send-string nil path-to-monolith)
		(process-send-string nil "bundle exec sidekiq\n")))

(defun run-ms-self-serve ()
  "Run the self-serve node application."
	(multi-vterm)
	(rename-buffer "ms-self-server client")
	(process-send-string nil "cd ~/Developer/ms-self-serve-ui\n")
	(process-send-string nil "yarn start\n"))

(defun ensure-postgres-running ()
	"Ensure postgres is running."
	; brew services start postgres@10
	)

(defun ensure-sidekiq-running ()
	"Ensure sidekiq is running."
	; bundle exec sidekiq
	)

(defun ensure-redis-running ()
	"Ensure redis is running."
	; brew services start redis
	)

(defun ensure-elasticsearch-running ()
	"Ensure elasticsearch is running."
	; brew services start elasticsearch@6
	)


(defun elasticsearch-repl ()
	"Elasticsearch scratch-pad split with repl."
	(interactive)
	;; TODO create a enw tab entitled "elasticsearch repl"
  (find-file "~/Developer/monolith/scratch.es")
	(split-window-right)
	(es-mode))

(defun postgres-repl ()
	"Postgres scratch pad split with repl."
	(interactive)
	;; TODO create a new tab entitled "postgres repl"
	(find-file "~/Developer/monolith/scratch.sql")
	(split-window-right)
	(sql-postgres))


(setq sql-postgres-login-params nil) 

(setq sql-connection-alist
			'((fountain-performance (sql-product 'postgres)
															(sql-database (getenv "PERF_PSQL_STRING")))
				(uat-staging (sql-product 'postgres)
										 (sql-database (getenv "UAT_PSQL_STRING")))
				(local-development (sql-product 'postgres)
													 (sql-database 'postgres)
													 (sql-database (concat "postgres://"
																								 "robertcarter"
																								 "@localhost"
																								 ":5432"
																								 "/db/onboardiq_dev")))))

(defun fountain-db ()
	"Convenience function to connect to postgres."
	(interactive)
	(find-file "~/Developer/monolith/scratch.sql")
	(split-window-right)
	(call-interactively #'sql-connect))

;(defalias 'fountain-db 'sql-connect)

(defun run-in-vterm-kill (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

(defun run-in-vterm (command)
  "Execute string COMMAND in a new vterm.

Interactively, prompt for COMMAND with the current buffer's file
name supplied.  When called from Dired, supply the name of the
file at point.

Like `async-shell-command`, but run in a vterm for full terminal features.

The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
  (interactive
   (list
    (let* ((f (cond (buffer-file-name)
                    ((eq major-mode 'dired-mode)
                     (dired-get-filename nil t))))
           (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
      (read-shell-command "Terminal command: "
                          (cons filename 0)
                          (cons 'shell-command-history 1)
                          (list filename)))))
  (with-current-buffer (vterm (concat "*" command "*"))
    (set-process-sentinel vterm--process #'run-in-vterm-kill)
    (vterm-send-string command)
    (vterm-send-return)))

(defun connect_monolith_multitenant ()
	"Convenient function to connect to multitenant rails console."
	(interactive)
	(with-current-buffer "*eshell*"
		(eshell-return-to-prompt)
		(insert "cd ~/Developer/terraform/aws/eks-clusters/production/us-east-1")
		(eshell-send-input)))


(defun enter-multitenant-pod ()
	(message "Opening multitenant")
	;(let ((path-to-terraform "/Users/robertcarter/Developer/terraform/aws/eks-clusters/production/us-east-1"))
	;	(multi-vterm)
	;	(rename-buffer "multitenant shell")

	;	(process-send-string nil path-to-terraform)
	;	(process-send-string nil "bundle exec rails server\n")

	;	(multi-vterm)
	;	(rename-buffer "monolith webpack")
	;	(process-send-string nil path-to-monolith)
	;	(process-send-string nil "yarn run webpack:development\n")
	)

(defun fountain-console ()
		 (interactive)
		 (let* ((choices '(("multi-tenant" . enter-multitenant-pod)))
						(choice (completing-read "Select instance: " choices)))
			 (call-interactively (cdr (assoc choice choices)))))


;(defun kubernetes-pods (candidate)
;	(cdr (assoc candidate '(("multi-tenant" . "Multitenant")
;													("staging uat" . "Staging")
;													("allegis" . "Allegis")
;													("amazon-na" . "Amazon North America")
;													("amazon-dsp" . "Amazon DSP (North America")
;													("chipotle" . "Mexican food for white people")
;													("instacart" . "Instacart")
;													("amazon-eu-dsp" . "Amazon DSP (Europe)")
;													("ceracare" . "Ceracare")
;													("takeawway" . "Takeaway")))))
;
;(defun fountain-console ()
;	(interactive)
;  (let ((completion-extra-properties '(:annotation-function kubernetes-pods)))
;		(completing-read "Choose: " '(("multi-tenant" . " Multi tenant environment")))))
	
