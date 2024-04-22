;; init.el --- Init file -*- lexical-binding: t -*-
;;; package --- Summary
;;; Commentary:
;;;
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
;;; use hydra package
;;; find anything useful in:  https://gitlab.com/ideasman42/dotfiles/-/blob/main/.config/emacs/init.el?ref_type=heads
;;; find anything useful in:  https://github.com/bling/dotemacs/tree/master
;;; find anything useful in:  https://github.com/meain/dotfiles/blob/master/emacs/.config/emacs/init.el#L1591-L1614
;;;
;;; Goal:
;;;        Investigate thread:  https://www.reddit.com/r/emacs/comments/td0nth/sample_usage_of_cape_completion_at_point/
;;;        Find out if I should use cape or company
;;;
;;; Goal:
;;;        find a way to have a history of commands in pipenv shell
;;;        https://stackoverflow.com/questions/6558765/how-do-you-see-the-entire-command-history-in-interactive-python
;;;
;;; Goal:
;;;        create a synchronous function that conects to a breezeway production instance
;;;        it should wait for commands to finish and parse the buffer for container ids


;;; Code:
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Remove frmae title and icon
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq frame-title-format '("\n"))
(setq ns-use-proxy-icon nil)

;; Disable frames resizing implicitly. Why?
;; Resizing the Emacs frame can be a terribly expensive part of changing the font.
;; By inhibiting this, we easily halve startup times with fonts that are larger than the system default.
(setq frame-inhibit-implied-resize t)

;; UTF-8 everywhere
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Session management
(require 'desktop)
(setq desktop-path (list "~/.emacs.d/sessions/"))
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

;; Highlight inner expression delineated by parentheses
(setq show-paren-style 'expression)

;; Remove undo-tree from completions
(setq completion-ignored-extensions
      (append completion-ignored-extensions
	      (quote
		  ("~undo-tree~"))))

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


;; Taken from perspective.el suggestions
;; Reuse windows as much as possible to minimize changes to layout
(customize-set-variable 'display-buffer-base-action
  '((display-buffer-reuse-window display-buffer-same-window)
    (reusable-frames . t)))

(customize-set-variable 'even-window-sizes nil)

;; Always reuse existing compilation window.
(push '("\\*compilation\\*" . (nil (reusable-frames . t))) display-buffer-alist)

;; Avoid prompt, just follow symbolic-links.
(setq vc-follow-symlinks t)

;; Hide frame border (called the fringe)
(set-fringe-mode 0)

; https://www.nathanfurnal.xyz/posts/building-tree-sitter-langs-emacs/
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
	(ruby  "https://github.com/tree-sitter/tree-sitter-ruby")
	(sql "https://github.com/m-novikov/tree-sitter-sql")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(push '(css-mode . css-ts-mode) major-mode-remap-alist)
(push '(python-mode . python-ts-mode) major-mode-remap-alist)
(push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
(push '(js-json-mode . json-ts-mode) major-mode-remap-alist)
(push '(typescript-mode . typescript-ts-mode) major-mode-remap-alist)

;; Tab bar settings
(setq tab-bar-new-button-show nil)
(set-face-attribute 'tab-bar nil :foreground "grey" :background 'unspecified)
(set-face-attribute 'tab-bar-tab nil :foreground "red2")
(set-face-attribute 'tab-bar-tab-inactive nil :foreground 'unspecified :background 'unspecified :box nil)
(set-face-attribute 'tab-bar-tab-group-inactive nil :foreground 'unspecified :background 'unspecified :box nil)

;; Do not clone current buffer into new tab
(setq tab-bar-new-tab-choice "*scratch*")

;; Hide tab-bar close button
(setq tab-bar-close-button-show nil) 

(setenv "PYTEST_ADDOPTS" "--color=yes")

(setq flymake-start-on-flymake-mode nil)

(setq manual-program "gman")

;; Set the modeline to show only the buffer name
;(setq-default mode-line-format
;    '("%e"
;	 (:eval (format "%s" (buffer-name)))
;	 "  "
;	 ;(:eval (list (nyan-create)))
;	 ))

(set-frame-font "JetBrains Mono:pixelsize=12")

;; config use-package
(eval-when-compile
  (require 'use-package))

;; install packages automatically if they are not present
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; prevent package.el loading packages prior to their init-file loading
(setq package-enable-at-startup nil)

(let ((gc-cons-threshold (* 256 1024 1024))
      (config-directory (concat user-emacs-directory "config/")))
  (unless (display-graphic-p) (menu-bar-mode -1))

  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("gnu" . "https://elpa.gnu.org/packages/")))
  (setq package-enable-at-startup nil)
  (package-initialize)

  ;; Place custom.el in config folder
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file))

  (cl-loop for file in (append (reverse (directory-files-recursively config-directory "\\.el$")))
           do (condition-case ex
                  (load (file-name-sans-extension file))
                ('error (with-current-buffer "*scratch*"
			  ; TDOO add to run bug-hunter on error
                          ;(insert (format "[INIT ERROR]\n%s\n%s\n\n" file ex))
			  (bug-hunter-file file)
			  )))))

;;; init.el ends here
