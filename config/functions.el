; Tab Related
;(defun new-named-tab ()
;	"Create a new named tab."
;	(interactive)
;	(call-interactively 'tab-new)
;	(tab-rename (read-string "Enter tab name: ")))
;
;(global-set-key (kbd "s-t") 'new-named-tab)
;
;(defun switch-project-with-new-tab ()
;	(interactive)
;  (tab-new)
;  (call-interactively 'projectile-switch-project)
;  (tab-rename (projectile-project-name)))

(defun unique-shell ()
	"Create a new named shell buffer."
  (interactive)
	;(call-interactively 'split-window-vertically)
	(call-interactively 'multi-vterm)
	(if (projectile-project-root)
			(process-send-string nil (concat "cd " (projectile-project-root) " \n")))
	(rename-buffer (concat (read-string "Enter name: ") (concat " (" (projectile-project-name) ")"))))

(defun spawn-shell (name)
	"Create a new shell buffer"
	(interactive "MName of the shell buffer to create: ")
	(pop-to-buffer (get-buffer-create (generate-new-buffer-name
																		 (shell (current-buffer))))))

(global-set-key (kbd "C-z") #'unique-shell)

(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

(defun robert/eshell-history (&optional initial-input)
	"Find command from eshell history. Initial-input can be given as the initial minibuffer input."
	(interactive)
	(insert
	  (completing-read "Find cmd: "
									 (robert/eshell-history-list))))

(defun robert/eshell-history-list ()
  "Return the eshell history as a list."
  (and (or (not (ring-p eshell-history-ring))
	   (ring-empty-p eshell-history-ring))
       (error "No history"))
  (let* ((index (1- (ring-length eshell-history-ring)))
	 (ref (- (ring-length eshell-history-ring) index))
	 (items (list)))
    (while (>= index 0)
      (setq items (cons (format "%s" (eshell-get-history index)) items)
	    index (1- index)
	    ref (1+ ref)))
    items))

; disable this so when embark is presented in the extended mini-buffer
; the cursor/active buffer is still embark and not the new buffer.  Without
; doing this the embark menu remains after the action and screws things up
;(defadvice split-window (after move-point-to-new-window activate)
;  "Moves the point to the newly created window after splitting."
;  (other-window 1))

(defun copy-filepath-to-clipboard ()
  "Put the current file name on the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun org-hide-properties ()
  "Hide all 'org-mode' headline property drawers in buffer.  Could be slow if it has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  (put 'org-toggle-properties-hide-state 'state 'hidden))

(defun org-show-properties ()
  "Show all 'org-mode' property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

(defun org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
      (org-show-properties)
    (org-hide-properties)))

(defun evil-org-insert-state-in-edit-buffer (fun &rest args)
  "Bind `evil-default-state' to `insert' before calling FUN with ARGS."
  (let ((evil-default-state 'insert)
        ;; Force insert state
        evil-emacs-state-modes
        evil-normal-state-modes
        evil-motion-state-modes
        evil-visual-state-modes
        evil-operator-state-modes
        evil-replace-state-modes)
    (apply fun args)
    (evil-refresh-cursor)))

(defun collect-search-in-split
  (split-window-right))
(advice-add 'embark-collect-live :before #'collect-search-in-split)

(push (list "open-profile-vsplit"
						(lambda ()
							(split-window-horizontally)
							(find-file "~/.zshrc")))
			vterm-eval-cmds)

(defun basic-auth-generator ()
	"Prompt for username & password and generate base64 encoded basic auth string."
	(interactive)
	(let ((username (read-string "username: "))
				(password (read-string "password: ")))
		(insert (concat "Basic " (base64-encode-string
											(concat username ":" password))))))

;; I want the ability to select an AWS EC2 instance from a list and connect to it.
;; I want to initiate this flow from a keybinding or from entering a function.

;; I want the ability to connect to a MySQL process from a list and connect to it.
;; likewise initiate from a keybinding or entering a function

;; I want the ability to list EC2 log files and possibly download them for
;; viewing in emacs

;; maybe can use 'shell-quote-argument' to escape quotes

;; https://www.reddit.com/r/emacs/comments/ovkyov/vterm_completion_for_files_directories_command/
;; https://emacs.stackexchange.com/questions/27407/accessing-json-data-in-elisp
;; https://stackoverflow.com/questions/35390729/how-to-return-the-value-instead-of-key-by-completing-read
;; need to parse JSON format

(defalias 'elisp-repl 'ielm)

(defun breezeway/start-ec2-session (instance-id)
	"Start EC2 Session from INSTANCE-ID.  Wrapper for aws ssm command."
	(multi-vterm)
	(rename-buffer (concat "EC2" instance-id))
	(process-send-string nil (concat  "aws ssm start-session --target " instance-id)))

(defun generate-name-id-tuples ()
	"Convert json format into more convenient form for further processing."
	(message "transforming json"))

(defun breezeway/select-ec2-instance ()
	"Prompt for ec2 instance to select."
	(interactive)
	(let* ((list-ec2-instances "aws ec2 describe-instances --filters \"Name=instance-state-name,Values=running\"  --query \"Reservations[*].Instances[*].{Instance:InstanceId,Name:Tags[?Key=='Name']|[0].Value}\" --output json")
				 (instances-json (shell-command-to-string list-ec2-instances)))
		(message instances-json)))

;		(breezeway/start-ec2-session (completing-read "ec2 instances" (split-string (shell-command-to-string list-ec2-instances) "\n" t)))))


