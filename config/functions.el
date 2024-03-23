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
