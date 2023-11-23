;;; package --- Summary
;;; Commentary:
;;;
;;; DEPENDENCIES
;;; oh-my-zsh
;;; MacTex
;;; nerd-fonts
;;; devicons
;;; Roboto Mono font
;;; JetBrains Mono Font
;;; ripgrep
;;; pngpaste
;;; pgformatter
;;; sqls
;;; exa
;;; man-db
;;; pip3 install epc
;;; pip3 install python-lsp-black
;;; pip3 install pyenv
;;; mandb (for creating man page db/cache) see: https://github.com/abo-abo/swiper/issues/2836#issuecomment-831292443
;;; graphviz (for google profiling tools)
;;; 
;;; Code:


;;; TODO
;;;  - Investigate https://emacs.stackexchange.com/questions/26226/looking-for-simple-bookmark-package
;;;     - maybe have to create own utility for bookmarks
;;;  - Investigate bookmark packages.. i want categories or some means of organizing them
;;;  - Understand why autocomplete doesn't work often
;;;  - Figure out how to disable autocomplete on delimieter characters like "," or ";"
;;;
;;;  - Add Embark Action for Identifier target that seraching org-roam notes with current mode as a TAG value
;;;      and identifier as the search term
;;;
;;;  - Create an Embark action that takes a Visual Region (highlighted words) and creates a new Org-Roam/Org document
;;;  
;;;  - Create a keybinding and function that takes the active buffer and places it in a new perspective
;;;
;;;  - Create a function that creates a project-relative python import path string for inserintg
;;;      - https://github.com/Wilfred/pyimport/blob/master/pyimport.el might have some useful code
;;;      - (file-relative-name buffer-file-name projectile-project-root)
;;;  - Figure out how to add ace-split action for ALL targets and for the scenario where I initiate a function like Help
;;;    but should have split first before finishing the Help/Doc command
;;;
;;;  - Create Embark Action for Region target that sends region to an interpreter, python repl for instance
;;;      "run-python" starts an inferior python process
;;;      "python-shell-send-region" sends current region to inferior python process
;;;      "process-send-region" (non interactive function)
;;;      https://emacs.stackexchange.com/questions/37887/send-region-to-shell-in-another-buffer
;;;
;;;  - use Emacs registers more:
;;;
;;;        window-configuration-to-register
;;;        jump-to-register
;;;        consult-register
;;;
;;;  - Figure out how to advise any jumping command to run `recenter-top-bottom` so the buffer is centered on the new location
;;;      - https://emacs.stackexchange.com/questions/14309/is-there-a-setting-to-automatically-center-the-text-after-any-jump
;;;      - https://stackoverflow.com/questions/11052678/emacs-combine-iseach-forward-and-recenter-top-bottom
;;;
;;;  - Create emacs save-hook that looks at current file and parses it for constructs like classes and function names
;;;    and attempts to find any relevant tests and automatically runs their tests
;;;
;;;  - Figure out how to use a git pre-commit hook to run pylint and integrate this output with magit
;;; 
;;;  - Figure out how to control or at least make predictable how Embark-Collect opens a target
;;;
;;;  - read this guide:  https://github.com/noctuid/evil-guide
;;;
;;;  - Figure out how to use snippets in vterm
;;;  - Investigate if a Yasnippet can be saved into a register, if it can then it might be possible to
;;;      load a register with the contents of a snippet and paste that into Vterm (instead of having to save to a temp buffer)
;;;  - Figure out how to configure LSP for projects (ignore node-modules etc)
;;;  - Create a command "repl" that lets me interactively select
;;;    a programming language REPL (i.e python, elisp, etc)
;;;  - Find out if buffers like magit-diff/status etc can be ignored by perspective
;;;     https://emacs.stackexchange.com/questions/59177/how-to-tell-persp-mode-to-ignore-some-buffers-by-major-mode
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
;;;  - https://reasonabledeviations.com/2023/02/05/gpt-for-second-brain/
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
;;;  - investigate config:   https://config.daviwil.com/emacs
;;;  - investigate goodies:  https://blog.sumtypeofway.com/posts/emacs-config.html
;;;
;;;  PACKAGES TO CONSIDER
;;;  guide:  https://github.com/emacs-tw/awesome-emacs
;;;
;;;  - consider:  https://github.com/jojojames/smart-jump
;;;  - consider:  https://github.com/minad/tempel
;;;  - consider:  https://github.com/Crandel/tempel-collection
;;;  - consider:  https://magit.vc/manual/forge/
;;;  - consider:  https://github.com/mickeynp/combobulate
;;;  - consider:  https://github.com/Shopify/ruby-lsp-rails
;;;  - consider:  https://github.com/postmodern/chruby
;;;  - consider:  https://github.com/plexus/chruby.el#readme
;;;  - consider:  https://github.com/dgutov/robe
;;;
;;;  - consider:  https://github.com/NicolasPetton/pass
;;;  - consider:  https://github.com/ch11ng/exwm
;;;               https://www.youtube.com/watch?v=MquoGuU8sHM
;;;
;;;  - consider:  https://github.com/armindarvish/consult-gh
;;;  - consider:  https://github.com/ronisbr/doom-nano-modeline
;;;  - consider:  https://github.com/karthink/popper
;;;  - consider:  https://github.com/4DA/eshell-toggle
;;;  - consider:  https://github.com/kyagi/shell-pop-el
;;;  - consider:  https://github.com/jessiehildebrandt/mood-line  (easy package to understand, maybe customize it or make my own package ?)
;;;  - consider:  https://www.emacswiki.org/emacs/MidnightMode
;;;  - consider:  https://github.com/Harith163/TransSide-theme
;;;  - consider:  https://elpa.nongnu.org/nongnu/paredit.html
;;;  - consider:  https://github.com/ch11ng/exwm/wiki
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

(setq straight-repository-branch "develop")

; install straight.el for git based packages
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(load "~/.emacs.d/config/appearance.el")
(load "~/.emacs.d/config/custom.el")
(load "~/.emacs.d/config/packages.el")
(load "~/.emacs.d/config/settings.el")
(load "~/.emacs.d/config/functions.el")
(load "~/.emacs.d/config/hooks.el")
(load "~/.emacs.d/config/keybindings.el")

; https://stackoverflow.com/questions/25125200/emacs-error-ls-does-not-support-dired
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(if (fboundp 'exec-path-from-shell-initialize)
		(when (memq window-system '(mac ns x))
			(exec-path-from-shell-initialize)))

; as recommended by perspective.el readme
(customize-set-variable 'display-buffer-base-action
  '((display-buffer-reuse-window display-buffer-same-window)
    (reusable-frames . t)))

(customize-set-variable 'even-window-sizes nil)     ; avoid resizing


(let ((fountain-scripts "~/.emacs.d/private/fountain/fountain.el"))
	(when (file-exists-p fountain-scripts)
		(load-file fountain-scripts)))

;;; init.el ends here
