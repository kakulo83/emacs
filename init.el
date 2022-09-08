;;; package --- Summary
;;; Commentary:
;;;
;;; DEPENDENCIES
;;; MacTex
;;; nerd-fonts
;;; ripgrep
;;; pngpaste
;;; pgformatter
;;; sqls
;;; exa
;;; man-db
;;; pip3 install epc
;;; 
;;; Code:


;;; TODO
;;;  - Find all references 'xref-find-references' is USELESS, it does not capture the entire word if it is separated with chars like '-'
;;;  - Figure a way to perform go-to-definition (gd) with the option of opening a vertical or horizontal split
;;;  - When opening a new projectile project, set emacs current working directory to root of project
;;;
;;;  - Refine searching
;;;  -   add ability to split result of consult-ripgrep in a vert or horizontal split
;;;  - create function to search for javascript/typescript files
;;;  - create function to search for ruby files
;;;  - add ability to search with ripgrep and show result in new split window
;;;
;;;  - investigate tree-sitter and whether i'm using it correctly
;;;
;;;  - Add new embark action for identity targets, option to search with lsp-fid-def in other window
;;;  - Investigate this emacs config:  https://ladicle.com/post/config/#doom-dracula-theme-modeline
;;; 
;;;  - consider:  https://github.com/radian-software/ctrlf 
;;;  - consider:  https://github.com/Silex/docker.el
;;;  - consider:  https://github.com/justbur/emacs-which-key
;;;  - consider:  https://github.com/alexluigit/dirvish
;;;  - consider:  https://github.com/jojojames/dired-sidebar
;;;  - consider:  https://github.com/lassik/emacs-format-all-the-code
;;;  - consider:  https://www.reddit.com/r/emacs/comments/ovkyov/vterm_completion_for_files_directories_command/
;;   - investigate https://issuecloser.com/blog/vterm-completion-for-files-directories-command-history-and-programs-in-emacs

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents)

(require 'use-package-ensure)

; quelpa allows installation of packages from source like github
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(setq custom-file "~/.emacs.d/config/custom.el")
(setq evil-want-C-u-scroll t) ; this needs to be executed before requiring 'evil

(load "~/.emacs.d/config/custom.el")
(load "~/.emacs.d/config/packages.el")
(load "~/.emacs.d/config/appearance.el")
(load "~/.emacs.d/config/settings.el")
(load "~/.emacs.d/config/functions.el")
(load "~/.emacs.d/config/hooks.el")
(load "~/.emacs.d/config/keybindings.el")

; https://stackoverflow.com/questions/25125200/emacs-error-ls-does-not-support-dired
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(exec-path-from-shell-copy-env "PERF_PSQL_STRING")
(exec-path-from-shell-copy-env "UAT_PSQL_STRING")
(exec-path-from-shell-copy-env "PROD_MULTI_TENANT_PRIMARY_PSQL_STRING")
(exec-path-from-shell-copy-env "PROD_MULTI_TENANT_SLAVE_PSQL_STRING")
(exec-path-from-shell-copy-env "URL_SHORTENER_PSQL_STRING")

; as recommended by perspective.el readme
(customize-set-variable 'display-buffer-base-action
  '((display-buffer-reuse-window display-buffer-same-window)
    (reusable-frames . t)))

(customize-set-variable 'even-window-sizes nil)     ; avoid resizing


(let ((fountain-scripts "~/.emacs.d/private/fountain/fountain.el"))
	(when (file-exists-p fountain-scripts)
		(load-file fountain-scripts)))

; add non package scripts
(load "~/.emacs.d/private/custom/persp-projectile.el")

;;; init.el ends here
