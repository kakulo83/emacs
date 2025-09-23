;; init.el --- Init file -*- lexical-binding: t -*-
;;; package --- Summary
;;; Commentary:
;;;
;;;   Zzzzz      |\      _,,,--,,_,
;;;              /,`.-'`'   ._  \-;;,__.
;;;             |,4-  ) )_   .;.(  `'___'
;;;            '---''(_/._)-'(_\_)
;;;
;;; ᓚᘏᗢ
;;; DEPENDENCIES
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
;;; man-db see:  https://github.com/abo-abo/swiper/issues/2836#issuecomment-831292443
;;;
;;; TODO
;;; find anything useful in:  https://gitlab.com/ideasman42/dotfiles/-/blob/main/.config/emacs/init.el?ref_type=heads
;;; find anything useful in:  https://github.com/bling/dotemacs/tree/master
;;; find anything useful in:  https://github.com/meain/dotfiles/blob/master/emacs/.config/emacs/init.el#L1591-L1614
;;; interesting package:      https://github.com/emarsden/pgmacs
;;; interesting package:      https://github.com/jxq0/org-tidy
;;;
;;; Investigate thread:  https://www.reddit.com/r/emacs/comments/td0nth/sample_usage_of_cape_completion_at_point/
;;; Find out if I should use cape or company
;;;
;;; 
;;; find a way to have a history of commands in pipenv shell
;;; https://stackoverflow.com/questions/6558765/how-do-you-see-the-entire-command-history-in-interactive-python
;;;
;;; 
;;; grab snippets from here:  https://gist.github.com/Ladicle/119c57fc97439c1b103f7847aa03be52?permalink_comment_id=4312513
;;; Make hydra menu pretty:  https://github.com/jerrypnz/major-mode-hydra.el?tab=readme-ov-file#pretty-hydra
;;;
;;; 
;;; look into dape debugger: https://www.youtube.com/watch?v=YKkyfz4cU8g
;;; https://github.com/svaante/dape?tab=readme-ov-file#configuration
;;; https://github.com/svaante/dape
;;; https://github.com/microsoft/debugpy
;;; 
;;; investigate this for performance tuning: https://www.leemeichin.com/posts/my-emacs-config.html
;;; 
;;; look into this https://github.com/ayrat555/company-elixir
;;; It would be cool to have Eglot available in the inferior-elixir-shell
;;; 
;;; Investigate this: https://github.com/rksm/org-ai
;;;   
;;;
;;; Investigate:  https://github.com/emacsmirror/undo-fu?tab=readme-ov-file
;;;                https://codeberg.org/ideasman42/emacs-undo-fu-session
;;;
;;; Look into eshell autocomplete https://elpa.gnu.org/packages/capf-autosuggest.html
;;; Disable ElixirLS elixirLS.autoInsertRequiredAlias don't want to auto add incorrect module aliases
;;;
;;; Consider alternative elixir language server:   https://www.elixir-tools.dev/docs/next-ls/editors/#emacs-with-lsp-mode
;;;
;;; Look into elixir tooling here:  https://github.com/Sasanidas/Apprentice?tab=readme-ov-file
;;; https://www.reddit.com/r/emacs/comments/wwr6h2/setting_up_snippets_and_auto_completion_with/
;;;
;;; Check out the prodigy package for managing external services:  https://github.com/rejeep/prodigy.el
;;; - Clean up Hydra Menus
;;; - find way to only show my snippets
;;; - find a way to get autocomplete options for shell commands... maybe copilot ?
;;;
;;; Check out https://github.com/radian-software/apheleia
;;;
;;; Emacs Config for web development:  https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config
;;;
;;; lsp-mode with multi server for major-mode:  https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config#orgc542f94
;;;
;;; worth reading through:  python setup:   https://blog.serghei.pl/posts/emacs-python-ide/
;;;
;;; another gpt package:  https://github.com/AnselmC/le-gpt.el
;;;
;;; desktop session alternatives:  https://www.reddit.com/r/emacs/comments/1hd5vco/sessions_in_emacs/
;;;
;;; Consider showing stuff on side:  https://github.com/emacs-sideline/sideline?tab=readme-ov-file
;;;
;;; Consider using https://github.com/federicotdn/verb as a substitute for restclient
;;;
;;; Consider Claude Code:  https://github.com/manzaltu/claude-code-ide.el
;;;
;;; Consider Macher:  https://github.com/kmontag/macher
;;;
;;; Try to speed up load:  https://github.com/jamescherti/minimal-emacs.d
;;;
;;; https://github.com/manzaltu/claude-code-ide.el?tab=readme-ov-file
;;; https://github.com/editor-code-assistant/eca-emacs?tab=readme-ov-file
;;; https://github.com/editor-code-assistant/eca
;;; https://www.reddit.com/r/emacs/comments/18rkds0/ai_coding_autocomplete_assistants/
;;;
;;; Create an Emacs function for formatting a region depending on the buffer type (json, html, etc)
;;; https://stackoverflow.com/questions/137043/can-emacs-re-indent-a-big-blob-of-html-for-me
;;; For html use the functions `sgml-pretty-print` and then `indent-for-tab` on the same region/buffer
;;;
;;; Code:

; enable package statistics
(setq use-package-compute-statistics t)

; needed due to failure in native-compilation
; https://github.com/d12frosted/homebrew-emacs-plus/issues/733
;(setenv "PATH" "/Users/robertcarter/.rbenv/shims:/Users/robertcarter/.rbenv/bin:/Users/robertcarter/.emacs.d/bin:/opt/homebrew/opt/postgresql@15/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/opt/homebrew/opt/mysql-client/bin:/Users/robertcarter/.nvm/versions/node/v20.11.1/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Users/robertcarter/.local/bin:/bin")
;(setq exec-path (split-string (getenv "PATH") path-separator))
(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Remove frame title and icon
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq frame-title-format '("\n"))
(setq ns-use-proxy-icon nil)

;; Disable frames resizing implicitly. Why?
;; Resizing the Emacs frame can be a terribly expensive part of changing the font.
;; By inhibiting this, we easily halve startup times with fonts that are larger than the system default.
(setq frame-inhibit-implied-resize t)

(setq inhibit-startup-echo-area-message t)

;; treat all themes as safe, stop prompting their potential risk
(setq custom-safe-themes t)

;; UTF-8 everywhere
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; enable delimeter pairing
(electric-pair-mode t)

;; enable window-divider mode so we can see the different splits
;(menu-bar-bottom-window-divider)

;; Enable encryption for Org Notes
;; https://emacs.stackexchange.com/questions/32881/enabling-minibuffer-pinentry-with-emacs-25-and-gnupg-2-1-on-ubuntu-xenial/68304#68304
(require 'epa-file)
(epa-file-enable)
(setq epg-pinentry-mode 'loopback)

;; Session management
(require 'desktop)
(setq desktop-path (list "~/.emacs.d/sessions/"))
;; Prevent desktop file from saving theme settings
;; https://superuser.com/questions/859761/prevent-emacs-desktop-save-from-holding-onto-theme-elements
(push '(foreground-color . :never) frameset-filter-alist)
(push '(background-color . :never) frameset-filter-alist)
(push '(font . :never) frameset-filter-alist)
(push '(cursor-color . :never) frameset-filter-alist)
(push '(background-mode . :never) frameset-filter-alist)
(push '(ns-appearance . :never) frameset-filter-alist)
(push '(background-mode . :never) frameset-filter-alist)
(desktop-save-mode 1)

;; Garbage-collect on focus-out, Emacs should feel snappier.
(unless (version< emacs-version "27.0")
  (add-function :after after-focus-change-function
                (lambda ()
                  (unless (frame-focus-state)
                    (garbage-collect)))))

(defmacro after (feature &rest body)
"Execute BODY after FEATURE has been loaded.

FEATURE may be any one of:
    'evil            => (with-eval-after-load 'evil BODY)
    \"evil-autoloads\" => (with-eval-after-load \"evil-autolaods\" BODY)
    [evil cider]     => (with-eval-after-load 'evil
                          (with-eval-after-load 'cider
                            BODY))
"
  (declare (indent 1))
  (cond
   ((vectorp feature)
    (let ((prog (macroexp-progn body)))
      (cl-loop for f across feature
               do
               (progn
                 (setq prog (append `(',f) `(,prog)))
                 (setq prog (append '(with-eval-after-load) prog))))
      prog))
   (t
    `(with-eval-after-load ,feature ,@body))))

(setq lisp-indent-offset 2)
(setq js-indent-level 2)
(setq js-jsx-indent-level 2)

; fix indent when inserting python snippets
(add-hook 'python-ts-mode-hook '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))

(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

;; tab-width is what eglot-format uses to indent the current mode
(setq-default tab-width 2)

;; do not wrap lines
(set-default 'truncate-lines t)

;; allow y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; disable dialog box
(setq use-dialog-box nil)

;; Disable backup.
(setq backup-inhibited t)

;; Disable auto save.
(setq auto-save-default nil)
;; Prevent creation of auto-save-list directory
(setq auto-save-list-file-prefix nil)

;; Disable splash screen
(setq inhibit-splash-screen t)

;; Show text instead of popups.
(setq use-dialog-box nil)

;; when we move outside the screen we always recenter
(setq scroll-conservatively scroll-margin)

;; set window margin
(setq-default left-margin-width 2 right-margin-width 0) ; Define new widths.
(set-window-buffer nil (current-buffer)) ; Use them now.

;; Scroll to first error.
(setq compilation-scroll-output 'first-error)

;; Always redraw immediately when scrolling,
;; more responsive and doesn't hang!
;; http://emacs.stackexchange.com/a/31427/2418
(setq fast-but-imprecise-scrolling nil)
(setq jit-lock-defer-time 0)

;; disable Emacs from ever displaying text right-to-left like arabic etc
(setq-default bidi-paragraph-direction 'left-to-right)

;; Don't show empty lines.
(setq indicate-empty-lines nil)

;; Don't show where buffer starts/ends.
(setq indicate-buffer-boundaries nil)

;; Quiet warnings
(setq ad-redefinition-action 'accept)

;; Never split windows
(setq split-width-threshold nil)

;; Don't put two spaces after full-stop.
(setq sentence-end-double-space nil)

;; Emacs redraw while scrolling tends to lazy-update,
;; at times this can *"get behind"*, causing annoying lags.
;; These settings favor immediate updates.

;; Scroll N lines to screen edge.
(setq scroll-margin 2)

;; Smoother scrolling with smaller steps
(setq scroll-step 1
      scroll-conservatively  10000)

;; Hide initial scratch message
(setq initial-scratch-message "")

;; Silence alert sound
(setq ring-bell-function 'ignore)

;; keep version control updated
(setq auto-revert-check-vc-info t)

;; Save bookmarks after every change
(setq bookmark-save-flag 1)

(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

;; Remove duplicate commands from history
(setq comint-input-ignoredups t)

;; Automatically revert buffers for changed files
(global-auto-revert-mode 1) 
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)


;; Highlight inner expression delineated by parentheses
(setq show-paren-style 'expression)

;; Remove undo-tree from completions
(setq completion-ignored-extensions
      (append completion-ignored-extensions
	      (quote
		  ("~undo-tree~"))))

;; experimental
(setq completion-in-region-function #'consult-completion-in-region)

(defconst my-num-processors (num-processors))
;; Avoid using too much memory.
(defconst my-num-processors-limited (/ my-num-processors 2))

;; Evil settings
;; allow C-u to perfrom evil scroll up, needs to be set before loading evil
(setq evil-want-C-u-scroll t)
(setq evil-want-fine-undo 'yes)
(setq evil-want-keybinding nil)
(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t))


(customize-set-variable 'even-window-sizes nil)

;; Set default ibuffer sorting
(setq ibuffer-default-sorting-mode 'filename/process)

;; Always reuse existing compilation window.
(push '("\\*compilation\\*" . (nil (reusable-frames . t))) display-buffer-alist)

;; Avoid prompt, just follow symbolic-links.
(setq vc-follow-symlinks t)

;; Disable prompt to save modified buffers
(set-buffer-modified-p nil)

;; Hide frame border (called the fringe)
(set-fringe-mode '(8 . 0))

;; Stop eldoc from echoing in minibuffer
(setq eldoc-echo-area-use-multiline-p nil)
(setq eldoc-echo-area-prefer-doc-buffer t)

;; Do not clone current buffer into new tab
(setq tab-bar-new-tab-choice "*scratch*")

;; Hide tab-bar close button
(setq tab-bar-close-button-show nil) 

(setenv "PYTEST_ADDOPTS" "--color=yes")

(setq flymake-start-on-flymake-mode nil)

(setq manual-program "gman")

;(add-to-list 'exec-path "~/.emacs.d/bin")
;(add-to-list 'exec-path "/opt/homebrew/bin")

;; Set the modeline to show only the buffer name
;(setq-default mode-line-format
;    '("%e"
;	 (:eval (format "%s" (buffer-name)))
;	 "  "
;	 ;(:eval (list (nyan-create)))
;	 ))

;; font size
;; https://www.reddit.com/r/emacs/comments/1ht83m1/choose_your_coding_font/
(set-face-attribute 'default nil :height 120)
;; font family
;(set-frame-font "JetBrains Mono")
(set-frame-font "-*-Roboto Mono-ultralight-normal-normal-*-*-*-*-*-m-0-iso10646-1")
;(set-frame-font "-*-Hack Nerd Font-regular-normal-normal-*-*-*-*-*-p-0-iso10646-1")
;(set-frame-font "-*-JetBrains Mono-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1")
;(set-frame-font "-*-Inconsolata Nerd Font-regular-normal-normal-*-*-*-*-*-p-0-iso10646-1")

; set clock for different timezones
(setq world-clock-list
      '(("America/Los_Angeles" "San Francisco")
        ("America/New_York" "New York")
        ("Europe/London" "London")
        ("Europe/Paris" "Paris")
        ("Europe/Frankfurt" "Frankfurt")
        ("Asia/Calcutta" "Bangalore")
        ("Asia/Tokyo" "Tokyo")))

;; config use-package
(eval-when-compile
  (require 'use-package))

;; install packages automatically if they are not present
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; prevent package.el loading packages prior to their init-file loading
(setq package-enable-at-startup nil)

;; performance improvements
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)
(setq native-comp-speed 2)
(setq native-comp-async-report-warnings-errors nil)


;; kill inactive buffers every 30 mins if they have been inactive for 30mins
(require 'midnight)

;;kill buffers if they were last disabled more than this seconds ago
(setq clean-buffer-list-delay-special 7200)

(defvar clean-buffer-list-timer nil
  "Stores `clean-buffer-list` timer if there is one.
You can disable `clean-buffer-list` by (cancel-timer clean-buffer-list-timer).")

;; run clean-buffer-list every 30mins 
(setq clean-buffer-list-timer (run-at-time t 10800 'clean-buffer-list))

;; kill everything, clean-buffer-list is very intelligent at not killing
;; unsaved buffer.
(setq clean-buffer-list-kill-regexps '("^.*$"))
(add-to-list 'clean-buffer-list-kill-regexps
             (rx buffer-start "magit-" (or "process" "diff")))

;; keep these buffer untouched
;; prevent append multiple times
(defvar clean-buffer-list-kill-never-buffer-names-init
  clean-buffer-list-kill-never-buffer-names
  "Init value for clean-buffer-list-kill-never-buffer-names.")
(setq clean-buffer-list-kill-never-buffer-names
      (append
       '("*Copilot* *vterm* *et~* *Messages*" "*EGLOT*" "*Inf*" "*shell*" "*Server*" "*chat*")
       clean-buffer-list-kill-never-buffer-names-init))

;; prevent append multiple times
(defvar clean-buffer-list-kill-never-regexps-init
  clean-buffer-list-kill-never-regexps
  "Init value for clean-buffer-list-kill-never-regexps.")
;; append to *-init instead of itself
(setq clean-buffer-list-kill-never-regexps
      (append '("^\\*EMMS Playlist\\*.*$")
	      clean-buffer-list-kill-never-regexps-init))


(let ((gc-cons-threshold most-positive-fixnum) ;(* 256 1024 1024))
      (config-directory (concat user-emacs-directory "config/")))
  (unless (display-graphic-p) (menu-bar-mode -1))

  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("gnu" . "https://elpa.gnu.org/packages/")))
  (setq package-enable-at-startup nil)
  (package-initialize)


; Configuration of exec-path-from-shell package	
; This sets $MANPATH, $PATH and exec-path from your shell, but only when executed
; in a GUI frame on OS X and Linux.
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
	(exec-path-from-shell-copy-envs '("LIBRARY_PATH" "INFOPATH" "CPATH" "MANPATH"))
	)


  (cl-loop for file in (append (reverse (directory-files-recursively config-directory "\\.el$")))
           do (condition-case ex
                  (load (file-name-sans-extension file))
                ('error (with-current-buffer "*scratch*"
			  ; TDOO add to run bug-hunter on error
                          ;(insert (format "[INIT ERROR]\n%s\n%s\n\n" file ex))
			  (bug-hunter-file file)
			  )))))

;; maximize emacs frame
;(toggle-frame-maximized)

(set-face-attribute 'fringe nil :background 'unspecified)

;; configure emacs for transparent background
;; (set-frame-parameter (selected-frame) 'alpha '(100 100))
;; (add-to-list 'default-frame-alist '(alpha 100 100))

; python shell autocompletes a bunch of crap that is annoying, this might be related
; https://emacs.stackexchange.com/questions/77297/python-shell-completion-complete-or-indent-tab-in-an-inferior-python-mode-shel

; configure ipython as the python shell
;(when (executable-find "ipython")
;  (setq python-shell-interpreter "ipython")
;	(setq python-shell-interpreter-args "-i --simple-prompt"))


; configure comint mode to load/save python history to ~/.python_history
;(defun my/comint-set-history-file (file)
;  "Load the given history FILE into Comint and write on process exit."
;  (setq comint-input-ring-file-name file)
;  (comint-read-input-ring t)
;  (set-process-sentinel (get-buffer-process (current-buffer))
;                        #'shell-write-history-on-exit))
;
;(defun my/set-python-repl-history-file ()
;  (my/comint-set-history-file (expand-file-name ".python_history" "~")))
;(add-hook 'inferior-python-mode-hook #'my/set-python-repl-history-file)
;;; init.el ends here
