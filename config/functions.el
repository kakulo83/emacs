;;; package --- Summary
;;; Commentary:
;;; Code:
(defun my/delete-buffer-or-workspace ()
"Delete the current buffer or workspace/tab."
  (interactive)
  (if (= (length (window-list)) 1)
      (call-interactively (tabspaces-close-workspace))
    (delete-window)))

(eval-when-compile
  (defmacro my/embark-ace-action (fn)
    `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
      (interactive)
      (with-demoted-errors "%s"
       (require 'ace-window)
       (let ((aw-dispatch-always t))
        (aw-switch-to-window (aw-select nil))
        (call-interactively (symbol-function ',fn)))))))

(defun my/cpm-open-notes-in-workspace ()
  "Open Notes in its own workspace."
  (interactive)
  (cond ((member "Notes" (tabspaces--list-tabspaces))
         (tab-bar-switch-to-tab "Notes"))
        (t
         (tab-bar-new-tab)
         (tab-bar-rename-tab "Notes")
         (message "TODO: open org notes")
	 (message "TODO: give this function a binding")
         )))


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


(defun robert/remove-empty-strs-from-list (list)
  "Remove empty string values from LIST."
  (--filter (not (string= "" it)) list))

(defvar notes-dir-path "/Users/robertcarter/Notes/org-roam-notes/")
(defvar rg-drill-cmd "rg -l '.*drill.*' ")

(defun robert/org-files-by-tag (tag)
  "Function to generate space separated string list of org files by TAG."
  (cd notes-dir-path)
  (let ((rg-tag-command (concat "rg -l 'tags:.*'" tag)))
    (string-join (split-string (shell-command-to-string rg-tag-command) "\n") " ")))

(defun robert/get-org-files-for-topic (tag)
  "Function to generate list of files from TAG."
  (interactive)
                                        ; rg -l '^#\+tags:.*database.*' | xargs rg -l '.*drill.*'
  (cd notes-dir-path)
                                        ; bind list of org files that have tag
  (let ((files-by-tag (robert/org-files-by-tag tag)))
                                        ; filter list further with onlly files that have drill items
    (split-string (shell-command-to-string (concat rg-drill-cmd files-by-tag)) "\n")))

(defun robert/drill-by-topic ()
  "Wrapper function on org-drill to invoke against a list of files from TOPIC."
  (interactive)
  (let* ((topic (read-string "Enter subject to drill: "))
	 (files (robert/get-org-files-for-topic topic)))
    (setq org-drill-scope (robert/remove-empty-strs-from-list files))
    (org-drill)))


;; Results of Embark Collection are displayed by invoking the compile-goto-error function.
;; Advise this function to do `display-buffer-in-side-window` instead
(defadvice compile-goto-error (around my-compile-goto-error activate)
  (let ((display-buffer-overriding-action '(display-buffer-in-side-window (inhibit-same-window . nil))))
    ad-do-it))


(autoload 'proced-process-attributes "proced" nil t)
(defun robert/quick-kill-process ()
  "Fuzzy search a list of active processes, choose, and kill the unresponsive target."
  (interactive)
  (let* ((pid-width 5)
         (comm-width 25)
         (user-width 10)
         (processes (proced-process-attributes))
         (candidates
          (mapcar (lambda (attributes)
                    (let* ((process (cdr attributes))
                           (pid (format (format "%%%ds" pid-width) (map-elt process 'pid)))
                           (user (format (format "%%-%ds" user-width)
                                         (truncate-string-to-width
                                          (map-elt process 'user) user-width nil nil t)))
                           (comm (format (format "%%-%ds" comm-width)
                                         (truncate-string-to-width
                                          (map-elt process 'comm) comm-width nil nil t)))
                           (args-width (- (window-width) (+ pid-width user-width comm-width 3)))
                           (args (map-elt process 'args)))
                      (cons (if args
                                (format "%s %s %s %s" pid user comm (truncate-string-to-width args args-width nil nil t))
                              (format "%s %s %s" pid user comm))
                            process)))
                  processes))
         (selection (map-elt candidates
                             (completing-read "kill process: "
                                              (seq-sort
                                               (lambda (p1 p2)
                                                 (string-lessp (nth 2 (split-string (string-trim (car p1))))
                                                               (nth 2 (split-string (string-trim (car p2))))))
                                               candidates) nil t)))
         (prompt-title (format "%s %s %s"
                               (map-elt selection 'pid)
                               (map-elt selection 'user)
                               (map-elt selection 'comm))))
    (when (y-or-n-p (format "Kill? %s" prompt-title))
      (if (eq (signal-process (map-elt selection 'pid) 9) 0)
          (message "killed: %s" prompt-title)
        (message "error: could not kill %s" prompt-title)))))

(defun robert/run-pytest-under-cursor (&optional full-file)
  "Run the nearest test using toffee.  Pass `FULL-FILE' to run all test in file."
  (interactive "P")
  (let ((test-file-name (buffer-file-name))
	 (line-number (line-number-at-pos)))
    (eshell-toggle)
    (eshell-return-to-prompt)
    (insert (shell-command-to-string
	      (format "toffee '%s' '%s'" test-file-name line-number)))
    (eshell-send-input)))

(defhydra hydra-test-runner ()
  "testing"
  ("p" robert/run-pytest-under-cursor "run pytest"))

(defhydra hydra-zoom ()
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(defhydra hydra-eglot ()
  "eglot"
  ("d" xref-find-definitions "xref definitions")
  ("r" xref-find-references "xref references")
  ("=" eglot-format-buffer "format")
  ("a" eglot-code-actions "apply code action"))

;; TODO create function to connect to production server
(defun connect-production ()
  "Connect to breezeway production server."
  ; if named eshell exists send command to instance
  ; if no eshell instances, create a new one and send command
  (with-current-buffer "eshell"
    (eshell-return-to-prompt)
    (insert "ls")
    (eshell-send-input))
  )
  
(provide 'functions)
;;; functions.el ends here
