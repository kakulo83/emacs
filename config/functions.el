(defun new-named-tab ()
	"Create a new named tab."
	(interactive)
	(call-interactively 'tab-new)
	(tab-rename (read-string "Enter tab name: ")))

(global-set-key (kbd "s-t") 'new-named-tab)

(defun switch-project-with-new-tab ()
	(interactive)
  (tab-new)
  (call-interactively 'projectile-switch-project)
  (tab-rename (projectile-project-name)))
  ;(tab-rename (read-string "Enter tab name: ")))

(defun unique-shell ()
	"Create a new named shell buffer."
  (interactive)
	;(call-interactively 'split-window-vertically)
	(call-interactively 'multi-vterm)
	;(call-interactively 'shell)
  (rename-buffer (read-string "Enter buffer name: ")))

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

(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

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
