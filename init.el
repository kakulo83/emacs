
;;; package --- Summary
;;; Commentary:
;;;
;;; DEPENDENCIES
;;; MacTex
;;; nerd-fonts
;;; Roboto Mono font
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
;;;  - Add ability to select visual region in any mode and create a new org-roam note from the region's contents
;;;      Simultaneously delete the selected content and replace it with a link to the new Org note
;;; 
;;;  - Figure out how to use snippets in vterm
;;;  - Investigate if a Yasnippet can be saved into a register, if it can then it might be possible to
;;;      load a register with the contents of a snippet and paste that into Vterm (instead of having to save to a temp buffer)
;;;  - Figure out how to configure LSP for projects (ignore node-modules etc)
;;;  - Create a command "repl" that lets me interactively select
;;;    a programming language REPL (i.e python, elisp, etc)
;;;  - Find out if buffers like magit-diff/status etc can be ignored by perspective
;;;      There is no value/reason for them to show up in the buffer-list
;;;  - Investigate:  https://github.com/jixiuf/vterm-toggle
;;;      This might be a solution to use evil to edit vterm commands etc
;;;  - Create my own theme
;;;      Use font from: https://www.labri.fr/perso/nrougier/GTD/index.html
;;;      Match grey colors from:  https://github.com/rougier/nano-emacs
;;;  - create Embark Action on an Identifier to perform a ripgrep search/export action
;;;  - dig for gems here:  https://www.reddit.com/r/emacs/comments/nr3cxv/what_are_your_very_useful_emacs_key_bindings_fast/
;;;  - Create my own modeline, extract from: https://github.com/jessiehildebrandt/mood-line/blob/master/mood-line.el
;;;  - Searching by Tags in Notes should be orderless:
;;;      example:  if searching for ec2, aws, breezeway, any order should match
;;;
;;;  TO LEARN
;;;  - learn how to use Embarks Export for grep results and how to perform additional actions like wgrep
;;;  - learn grep-mode
;;;
;;;  SNIPPETS TO TRY
;;;
;;;  - GREAT EXAMPLE:   https://www.reddit.com/r/emacs/comments/ovkyov/vterm_completion_for_files_directories_command/
;;;
;;;  - Investigate my magit status bug:  https://github.com/magit/magit/issues/4744
;;;                                      https://github.com/magit/magit/issues/4739
;;;  - investigate https://issuecloser.com/blog/vterm-completion-for-files-directories-command-history-and-programs-in-emacs
;;;  - investigate this emacs config:  https://ladicle.com/post/config/#doom-dracula-theme-modeline
;;;  - investigate this dudes config: https://github.com/jakebox/jake-emacs
;;;      - he uses the cape package and corfu
;;;  - Investigate:  https://github.com/akermu/emacs-libvterm/issues/313#issuecomment-738842507
;;;      Maybe some of these functions can be used to use evil in vterm commandline
;;;  - investigate: https://localauthor.github.io/posts/aw-select.html
;;;
;;;  PACKAGES TO CONSIDER
;;;  - consider:  https://gitlab.com/niklaseklund/dtache
;;;  - consider:  https://github.com/jgru/consult-org-roam
;;;  - consider:  creating a documentation system within emacs (https://hynek.me/articles/productive-fruit-fly-programmer/)
;;;  - consider:  https://elpa.gnu.org/packages/devdocs.html
;;;  - consider:  https://github.com/dash-docs-el/dash-docs
;;;  - consider:  https://github.com/radian-software/ctrlf
;;;  - consider:  https://github.com/Silex/docker.el
;;;  - consider:  https://github.com/justbur/emacs-which-key
;;;  - consider:  https://github.com/alexluigit/dirvish
;;;  - consider:  https://github.com/lassik/emacs-format-all-the-code
;;;  - consider:  https://www.reddit.com/r/emacs/comments/ovkyov/vterm_completion_for_files_directories_command/
;;;  - consider:  https://www.emacswiki.org/emacs/AutoInsertMode

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
(load "~/.emacs.d/private/persp/persp-projectile.el")

;;; init.el ends here
