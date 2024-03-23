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
;;; 
;;;
;;; TODO 
;;; create a simple organization scheme for loading elisp files
;;; add packages
;;; use display-buffer-alist
;;; use hydra package
;;; find anything useful in:  https://gitlab.com/ideasman42/dotfiles/-/blob/main/.config/emacs/init.el?ref_type=heads
;;; find anything useful in:  https://github.com/bling/dotemacs/tree/master

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; Disable frames resizing implicitly. Why?
;; Resizing the Emacs frame can be a terribly expensive part of changing the font.
;; By inhibiting this, we easily halve startup times with fonts that are larger than the system default.
(setq frame-inhibit-implied-resize t)

;; UTF-8 everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Garbage-collect on focus-out, Emacs should feel snappier.
(unless (version< emacs-version "27.0")
  (add-function :after after-focus-change-function
                (lambda ()
                  (unless (frame-focus-state)
                    (garbage-collect)))))

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

;; Always reuse existing compilation window.
(push '("\\*compilation\\*" . (nil (reusable-frames . t))) display-buffer-alist)

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

(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

;; Highlight inner expression delineated by parentheses
(setq show-paren-style 'expression)

;; Do not clone current buffer into new tab
(setq tab-bar-new-tab-choice "*scratch*")

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

;; Kill scratch buffer on startup
;(kill-buffer "*scratch*")

;; Taken from perspective.el suggestions
;; Reuse windows as much as possible to minimize changes to layout
(customize-set-variable 'display-buffer-base-action
  '((display-buffer-reuse-window display-buffer-same-window)
    (reusable-frames . t)))

(customize-set-variable 'even-window-sizes nil)

;; Avoid prompt, just follow symbolic-links.
(setq vc-follow-symlinks t)


;; config use-package
(eval-when-compile
  (require 'use-package))

;; install packages automatically if they are not present
(require 'use-package-ensure)
(setq use-package-always-ensure t)


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
                          (insert (format "[INIT ERROR]\n%s\n%s\n\n" file ex)))))))

;;; init.el ends here
