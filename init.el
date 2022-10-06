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
;;;  - Figure out how to use snippets in vterm
;;;  - Figure out how to configure LSP for projects (ignore node-modules etc)
;;;  - When opening a new projectile project, set emacs current working directory to root of project
;;;  - Add ability to search with ripgrep and show result in new split window
;;;  - Make Embark-Collect default to showing results in a vertical split
;;;  - Create a command "repl" that lets me interactively select
;;;    a programming language REPL (i.e python, elisp, etc)
;;;  - Find out if buffers like magit-diff/status etc can be ignored by perspective
;;;      There is no value/reason for them to show up in the buffer-list
;;;  - Add an Embark identifier action to open a filepath from vterm into a regular Buffer (open the file in other words)
;;;  - Investigate:  https://github.com/jixiuf/vterm-toggle
;;;      This might be a solution to use evil to edit vterm commands etc
;;;  - Investigate:  https://github.com/akermu/emacs-libvterm/issues/313#issuecomment-738842507
;;;      Maybe some of these functions can be used to use evil in vterm commandline
;;;  - Add ability to select visual region in Org mode and create a new note from the region's contents
;;;      Simultaneously delete the selected content and replace it with a link to the new Org note
;;;  - Create my own theme
;;;      Use font from: https://www.labri.fr/perso/nrougier/GTD/index.html
;;;      Match grey colors from:  https://github.com/rougier/nano-emacs
;;;
;;;  - consider:  creating a documentation system within emacs (https://hynek.me/articles/productive-fruit-fly-programmer/)
;;;  - consider:  https://elpa.gnu.org/packages/devdocs.html
;;;  - consider:  https://github.com/dash-docs-el/dash-docs
;;;  - consider:  https://github.com/radian-software/ctrlf
;;;  - consider:  https://github.com/Silex/docker.el
;;;  - consider:  https://github.com/justbur/emacs-which-key
;;;  - consider:  https://github.com/alexluigit/dirvish
;;;  - consider:  https://github.com/lassik/emacs-format-all-the-code
;;;  - consider:  https://www.reddit.com/r/emacs/comments/ovkyov/vterm_completion_for_files_directories_command/
;;;  - investigate https://issuecloser.com/blog/vterm-completion-for-files-directories-command-history-and-programs-in-emacs
;;;  - investigate this emacs config:  https://ladicle.com/post/config/#doom-dracula-theme-modeline
;;;  - investigate this dudes config: https://github.com/jakebox/jake-emacs
;;;      - he uses the cape package and corfu

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents)

(require 'use-package-ensure)

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
