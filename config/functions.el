(defun my/delete-buffer-or-workspace ()
  "Delete the current buffer or if only buffer left in workspace delete the workspace/tab."
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


;; Results of Embark Collection are displayed by invoking the compile-goto-error function.
;; Advise this function to do `display-buffer-in-side-window` instead
(defadvice compile-goto-error (around my-compile-goto-error activate)
  (let ((display-buffer-overriding-action '(display-buffer-in-side-window (inhibit-same-window . nil))))
    ad-do-it))


(defhydra hydra-zoom ()
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))


(defhydra hydra-magit ()
   "magit"
   ("s" magit-status "status")
   ("b" magit-blame "blame")
   ("l" magit-log "log")
   ("c" magit-checkout "checkout")
   ("C" magit-commit "commit")
   ("P" magit-push "push")
   ("F" magit-fetch "fetch")
   ("M" magit-merge "merge")
   ("R" magit-rebase "rebase")
   ("t" magit-tag "tag")
   ("T" magit-tag-delete "delete tag")
   ("r" magit-revert "revert")
   ("d" magit-diff "diff")
   ("D" magit-diff-buffer-file "diff buffer")
   ("f" magit-find-file "find file")
   ("L" magit-list-repositories "list repositories")
   ("q" nil "quit"))
