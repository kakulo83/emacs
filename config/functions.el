;;; package --- Summary -*- lexical-binding: t -*-
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

;(advice-add 'evil-goto-mark-line :after #'recenter-top-bottom)
(defun scroll-to-center-advice (&rest args)
  (evil-scroll-line-to-center (line-number-at-pos)))
(advice-add #'evil-goto-line :after #'scroll-to-center-advice)
(advice-add #'better-jumper-jump-backward :after #'scroll-to-center-advice)
(advice-add #'better-jumper-jump-forward  :after #'scroll-to-center-advice)
(advice-add #'xref-find-definitions :after #'scroll-to-center-advice)


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



; https://www.reddit.com/r/emacs/comments/ynbbu7/store_autoyasnippets_in_registers_and_expand_on/
(defun robert/yasnippet-insert ()
  "Insert a yasnippet.  If buffer is vterm, send to vterm."
  (interactive)
  (if (eq major-mode 'vterm-mode)
    (message "inserting vterm snippet")
    (yas-insert-snippet)))
    ;(robert/vterm-insert-snip (yas-choose-value (yas--all-templates)))


(defun robert/vterm-insert-snip (str)
  "Insert a snippet into the vterm buffer."
  (interactive)
  (let* ((inhibit-read-only t))
    (vterm-send-string str nil)))
  ;(vterm-send-string (concat "echo " (yas-choose-value (yas--all-templates)) " \n")))


(defun delete-visible-buffers ()
  "Delete all visible buffers showing in windows."
	(interactive)
  (let ((visible-buffers (delete-dups (mapcar #'window-buffer (window-list)))))
    (dolist (buffer visible-buffers)
      (kill-buffer buffer))))


(defun save-buffers-to-register-and-close ()
	"Save all buffers to a register, close, and create an empty one."
	(interactive)
	(window-configuration-to-register ?x) ; NOTE the question mark symbol is for referencing registers
	(delete-other-windows)
	(switch-to-buffer (generate-new-buffer "Scratch Pad")))


(defhydra hydra-window-utils (:color green :hint nil)
  "
Window misc
------------
_n_: toggle line numbers   _b_: balance windows     _k_: kill frame          _+_: increase font
_c_: copy buffer path                             _p_: new frame           _-_: decrease font
_f_: full-screen           _x_: temp workspace
"
  ("c" copy-filepath-to-clipboard :exit t)
  ("f" toggle-frame-maximized :exit t)
  ("n" global-display-line-numbers-mode :exit t)
  ("k" delete-frame)
	("p" make-frame)
	("b" balance-windows-area)
	("x" save-buffers-to-register-and-close)
  ("+" text-scale-increase)
  ("-" text-scale-decrease))

(defhydra hydra-eglot (:hint nil)
  "
Eglot Actions
--------------
_s_: start
_d_: go definition     _=_: format buffer      _X_: shutdown all
_r_: find references   _a_: apply code action
_R_: rename
"
  ("s" eglot :exit t)
  ("d" xref-find-definitions)
  ("r" xref-find-references)
  ("R" eglot-rename)
  ("=" eglot-format-buffer)
  ("a" eglot-code-actions)
  ("X" eglot-shutdown-all :exit t))

(defun robert/find-snippet-by-name ()
  (interactive)
  (yas-expand-snippet (yas-lookup-snippet "name-of-your-snippet")))

(defhydra hydra-snippets (:hint nil)
  "
Snippet Actions
-----------------
_i_: insert   _n_: new    _l_: list all snippets
_r_: reload   _e_: edit   _f_: find by name
"
  ("i" robert/yasnippet-insert :exit t)
  ("f" robert/find-snippet-by-name :exit t)
  ("n" yas-new-snippet :exit t)
  ("e" yas-visit-snippet-file :exit t)
  ("r" yas-reload-all)
  ("l" yas-describe-tables))

(defhydra hydra-repl (:color green :hint nil)
  "
Inferior REPL
---------------
_e_: elixir
_p_: python
_r_: ruby
_n_: node
"
  ("e" inf-elixir :exit t)
  ("p" run-python :exit t)
  ("r" inf-ruby :exit t)
  ("n" nodejs-repl :exit t))

;(defun robert/embark-clear-register ()
;  "This function is meant to be invoked from embark. It clears a register."
;  (interactive)
;  (message "clearing register "))

(defhydra hydra-register ()
  "register"
  ("s" window-configuration-to-register "save")
  ;("d" (lambda ()
  ;	 (interactive)
  ;	 ((let ((register (string-to-char (read-string "select register: "))))
  ;	    (set-register ?register nil)))) "delete")
  ("l" consult-register-load "load"))


;; following from https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/
(defvar robert/copilot-manual-mode nil
  "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")


(defun robert/copilot-change-activation ()
  "Switch between three activation modes:
    - automatic: copilot will automatically overlay completions
    - manual: you need to press a key (M-C-<return>) to trigger completions
    - off: copilot is completely disabled."
  (interactive)
  (if (and copilot-mode robert/copilot-manual-mode)
    (progn
      (message "deactivating copilot")
      (global-copilot-mode -1)
      (setq robert/copilot-manual-mode nil))
    (if copilot-mode
      (progn
	(message "activating copilot manual mode")
	(setq robert/copilot-manual-mode t))
      (message "activating copilot mode")
      (copilot-mode))))


(defun robert/copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one is available. Useful if you tend to hammer your keys like I do."
  (interactive)
  (if (copilot--overlay-visible)
    (progn
      (copilot-accept-completion)
      (open-line 1)
      (next-line))
    (copilot-complete)))


(defhydra hydra-vc (:color green :hint nil)
  "vc"
  ("a" vc-annotate "Annotate" :exit t)
  ("b" magit-blame "Blame" :exit t)
  ("d" vc-diff "Diff" :exit t)
  ("f" magit-find-file "Find file" :exit t)
  ("H" git-timemachine "Time-machine" :exit t)
  ("l" magit-log-buffer-file "File log" :exit t)
  ("L" magit-log-all "Global log" :exit t)
  ("s" magit-status "Status" :exit t)
  ("u" git-link "link" :exit t)
  )

(defhydra hydra-flycheck (:color red :hint nil)
  "
Actions
----------
_f_: activate flyover
_l_: list
_n_: next error
_p_: prev error
"
	("f" flyover-mode :exit t)
  ("l" flycheck-list-errors :exit t)
  ("n" flycheck-next-error)
  ("p" flycheck-previous-error))

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
  
(defun robert/tab ()
  "Command to complete a copilot suggestion if available otherwise insert a tab."
  (interactive)
  (or (copilot-accept-completion-by-word)
    (indent-for-tab-command)))

(defun robert/open-notes-dired-in-tab()
  "Open a new tab with notes."
  (interactive)
  (tab-bar-new-tab)
  (tab-bar-rename-tab "Notes")
  (find-file "~/Notes/org-roam-notes/")
  (call-interactively 'project-find-file))

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

(defun robert/unique-vterm-shell ()
  "Create a new named vterm buffer."
  (interactive)
  (call-interactively 'multi-vterm)
  (if (vc-root-dir)
    (vterm-send-string (concat "cd " (vc-root-dir) " \n"))))
  ;(rename-buffer (concat (read-string "Enter name: ") (concat " (" (project-name) ")")))))
      ;(process-send-string nil (concat "cd " (vc-root-dir) " \n")))
  ;(rename-buffer (concat (read-string "Enter name: ") (concat " (" (project-name) ")"))))



; https://github.com/meain/toffee/blob/master/src/pickers/python.rs

(setq meain/tree-sitter-calss-like '((rust-mode . (impl_item))
				      (python-mode . (class_definition))))
(setq meain/tree-sitter-function-like '((rust-mode . (function_item))
					 (go-mode . (function_declaration method_declaration))
					 (python-mode . (function_definition))))

(defun meain/tree-sitter-thing-name (kind)
  "Get name of tree-sitter THING-KIND."
  (if tree-sitter-mode
    (let* ((node-types-list (pcase kind
			      ('class-like meain/tree-sitter-calss-like)
			      ('function-like meain/tree-sitter-function-like)))
	    (node-types (alist-get major-mode node-types-list)))
      (if node-types
	(let ((node-at-point (car (remove-if (lambda (x) (eq nil x))
				    (seq-map (lambda (x) (tree-sitter-node-at-point x))
				      node-types)))))
	  (if node-at-point
	    (let ((node-name-node-at-point (tsc-get-child-by-field node-at-point ':name)))
	      (if node-name-node-at-point
		(tsc-node-text node-name-node-at-point)))))))))


(defun robert/embark-org-roam-cut-to-new-note (start end)
  "Cut region and populate new org-roam note."
  (interactive "r")
  (let* ((text (delete-and-extract-region start end))
	  (tags (read-string "Enter tags: "))
	  (title (read-string "Title of note: "))
	  (slug (org-roam-node-slug (org-roam-node-create :title title)))
	  (filename (format "%s/%d-%s.org"
		      (expand-file-name org-roam-directory)
		      (time-convert (current-time) 'integer)
		      slug))
	  (org-id-overriding-file-name filename)
	  id)
    (with-temp-buffer
      (insert ":PROPERTIES:\n:ID:        \n:END:\n#+title: "
	title)
      (goto-char 25)
      (setq id (org-id-get-create))
      (goto-char (point-max))
      (insert "\n#+startup: showall inlineimages")
      (insert "\n#+tags:")
      (insert "\n#+options: ^:{}")
      (goto-char (point-max))
      (insert "\n\n")
      (goto-char (point-max))
      (insert text)
      (write-file filename)
      (org-roam-db-update-file filename)
      (format "[[id:%s][%s]]" id title))
      ; insert link in place of moved text
    (insert (concat "[[id:" id "][" title "]]"))))

(defalias 'dired-refresh 'revert-buffer)

;; todo add tooling for sql
;; https://arjanvandergaag.nl/blog/using-emacs-as-a-database-client.html

(provide 'functions)
;;; functions.el ends here
