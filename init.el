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
;;; 
;;; Code:


;;; TODO
;;;  - Refine searching
;;;  -   add ability to split result of consult-ripgrep in a vert or horizontal split
;;;  - create function to search for javascript/typescript files
;;;  - create function to search for ruby files
;;;  - add ability to search with ripgrep and show result in new split window
;;;
;;;  - investigate tree-sitter and whether i'm using it correctly
;;;
;;;  - <leader> q should close the target window if many windows, a tab if only one window, or a frame if only one window

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

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(exec-path-from-shell-copy-env "PERF_PSQL_STRING")
(exec-path-from-shell-copy-env "UAT_PSQL_STRING")
(exec-path-from-shell-copy-env "PROD_MULTI_TENANT_PRIMARY_PSQL_STRING")
(exec-path-from-shell-copy-env "PROD_MULTI_TENANT_SLAVE_PSQL_STRING")
(exec-path-from-shell-copy-env "URL_SHORTENER_PSQL_STRING")


(customize-set-variable 'display-buffer-base-action
  '((display-buffer-reuse-window display-buffer-same-window)
    (reusable-frames . t)))

(customize-set-variable 'even-window-sizes nil)     ; avoid resizing


(let ((fountain-scripts "~/.emacs.d/private/fountain/fountain.el"))
	(when (file-exists-p fountain-scripts)
		(load-file fountain-scripts)))

;;; init.el ends here
