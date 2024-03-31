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
