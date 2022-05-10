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
;;;  - get reliable ripgrep functionality
;;;  - figure out how to use ace-windows to split
;;;  - figure out how to combine embark and ace-windows

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents)

(require 'use-package-ensure)

(setq evil-want-C-u-scroll t) ; this needs to be executed before requiring 'evil

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


(customize-set-variable 'display-buffer-base-action
  '((display-buffer-reuse-window display-buffer-same-window)
    (reusable-frames . t)))

(customize-set-variable 'even-window-sizes nil)     ; avoid resizing


(let ((fountain-scripts "~/.emacs.d/private/fountain/fountain.el"))
	(when (file-exists-p fountain-scripts)
		(load-file fountain-scripts)))

;; https://www.gnu.org/software/emacs/manual/html_node/eshell/Input_002fOutput.html#Input_002fOutput 
;;(add-to-list 'eshell-visual-commands "git")
;;(add-to-list 'eshell-visual-commands "rails")

;; =======================================================================================================================================================================================================================================================
;;; init.el ends here
