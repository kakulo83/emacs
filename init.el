;; package --- Summary
;;; Commentary:
;;;
;;; DEPENDENCIES
;;; MacTex
;;; ripgrep
;;; pngpaste
;;; pgformatter
;;; sqls
;;;
;;; Code:

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents)

(require 'use-package-ensure)

(defvar evil-want-C-u-scroll)
(defvar ac-cons-threshold)
(defvar all-the-icons-dired-monochrome)
(defvar show-paren-style)
(defvar evil-want-fine-undo)
(defvar eshell-prompt-function)
(defvar exec-path-from-shell-initialize)
(defvar exec-path-from-shell-variables)

(setq use-package-always-ensure t)
(setq warning-minimum-level :error)
(setq use-dialog-box nil)

;(setq sql-postgres-login-params
;      '((user :default "robertcarter")
;        (database :default "db/onboardiq_dev")
;        (server :default "localhost")
;        (port :default 5432)))
			
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" default))
 '(display-buffer-base-action
	 '((display-buffer-reuse-window display-buffer-same-window)
		 (reusable-frames . t)))
 '(embark-prompter 'embark-completing-read-prompter)
 '(even-window-sizes nil)
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
 '(help-at-pt-display-when-idle '(flymake-diagnostic) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.1)
 '(package-selected-packages
	 '(balanced-windows org-roam-ui sly kubel native-complete pdf-view-restore pdf-tools yasnippet-snippets elpy lsp-pyright org-drill doom-themes

																						es es-mode es multi-vterm rvm vterm projectile-rails auctex org-download undo-tree websocket sqlformat olivetti consult-selectrum cider project rg simple-httpd helpful org-bullets org-roam company yasnippet embark-consult embark marginalia consult rainbow-delimiters orderless dashboard company-box org lsp-ui go-mode bug-hunter use-package))
 '(warning-suppress-types '((org-roam) (org-roam))))

(customize-set-variable 'display-buffer-base-action
  '((display-buffer-reuse-window display-buffer-same-window)
    (reusable-frames . t)))

(customize-set-variable 'even-window-sizes nil)     ; avoid resizing

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :foreground "#A0B3C5" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Inconsolata Light"))))
 '(italic ((t (:foreground "white" :slant italic))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "ETBembo" :height 6.0 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "ETBembo" :height 2.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "ETBembo" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "ETBembo" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "ETBembo" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "ETBembo"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "ETBembo"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "ETBembo"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "ETBembo")))))

(setq lexical-binding t)
(setq evil-want-C-u-scroll t)
(setq evil-want-fine-undo 'yes)
(setq initial-scratch-message "")
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      ; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
      ) ;; increase garbage-collection threshold to 100mb from 8kb
(setq all-the-icons-dired-monochrome nil)
(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

(fset 'yes-or-no-p 'y-or-n-p)  ;; use 'y' and 'n' for 'yes' and 'no'

(setq completion-ignore-case t)

(let ((fountain-scripts "~/.emacs.d/private/fountain/fountain.el"))
	(when (file-exists-p fountain-scripts)
		(load-file fountain-scripts)))

;; PACKAGES ==============================================================================================================================================================================================================================================

(use-package bug-hunter)

(use-package autothemer)

(use-package projectile
	:config
	(setq projectile-switch-project-action 'projectile-dired)
	:init
	(projectile-mode +1))

(use-package evil
	:functions org-roam-capture org-roam-capture
  :init
	(setq evil-want-keybinding nil)
	:config
	(define-key evil-normal-state-map (kbd "u") nil)
	(define-key evil-motion-state-map (kbd "C-z") nil)
	(define-key evil-motion-state-map (kbd "RET") nil)
	(define-key evil-motion-state-map (kbd "C-f") nil)
	(define-key evil-normal-state-map (kbd "-") 'dired)
	(define-key evil-normal-state-map (kbd "C-t") 'tab-bar-switch-to-tab)
	(define-key evil-normal-state-map (kbd "C-s") 'projectile-switch-project)
	(define-key evil-normal-state-map (kbd "C-p") 'project-find-file)
	(define-key evil-normal-state-map (kbd "C-b") 'switch-to-buffer)
	(define-key evil-normal-state-map (kbd "C-n") 'treemacs)
	(define-key evil-normal-state-map (kbd "z-c") 'hs-hide-block)
	(define-key evil-normal-state-map (kbd "z-o") 'hs-show-block)
	(define-key evil-normal-state-map (kbd "C-c n") 'org-roam-capture)
	(define-key evil-normal-state-map (kbd "/") 'consult-line)
	(define-key evil-motion-state-map (kbd "n") 'isearch-repeat-forward)
	(define-key evil-motion-state-map (kbd "N") 'isearch-repeat-backward)
	(evil-define-key 'normal org-mode-map (kbd "C-j") 'evil-window-down)
	(evil-define-key 'normal org-mode-map (kbd "C-k") 'evil-window-up)
	(evil-define-key 'normal eshell-mode-map (kbd "C-n") 'treemacs)
	(evil-define-key 'normal treemacs-mode-map (kbd "s") 'treemacs-visit-node-horizontal-split)
	(evil-define-key 'normal treemacs-mode-map (kbd "i") 'treemacs-visit-node-vertical-split)
	(evil-define-key 'normal vterm-mode-map (kbd "C-z") 'unique-shell)
	(evil-define-key 'normal pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
	(evil-define-key 'normal pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
	(evil-define-key 'normal pdf-view-mode-map (kbd "d") 'pdf-view-next-page)
	(evil-define-key 'normal pdf-view-mode-map (kbd "u") 'pdf-view-previous-page)
	(evil-define-key 'normal treemacs-mode-map (kbd "a") 'treemacs-create-file)
	(evil-define-key 'normal treemacs-mode-map (kbd "m") 'treemacs-move-file)
	(evil-define-key 'normal treemacs-mode-map (kbd "d") 'treemacs-delete)
	(evil-define-key 'normal treemacs-mode-map (kbd "r") 'treemacs-refresh)
  (evil-mode))

(use-package evil-collection
	:defines evil-collection-company-use-tng
	:after evil
	:config
	(setq evil-collection-mode-list
				'(dired
					dashboard
					magit
					proced
					help
					man
					woman
					completion
					helpful))
	(setq evil-collection-company-use-tng nil)
	(evil-collection-init))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package selectrum
	:functions selectrum-mode
	:config
	(setq selectrum-max-window-height (/ (frame-height) 2))
	(setq selectrum-fix-vertical-window-height t)
	(selectrum-mode +1))

(use-package lsp-mode
	:commands (lsp lsp-deferred)
	:config
  (setq lsp-lens-enable t)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-keymap-prefix "C-c l")
	(setq lsp-headerline-breadcrumb-enable nil)
  :init
  :hook (
				 (sql-mode . lsp-deferred)
				 (ruby-mode . lsp-deferred)
				 (js-mode . lsp-deferred)
				 (clojure-mode . lsp-deferred)
				 (c-mode . lsp-deferred)
									 )
	:bind (:map lsp-mode-map
							("TAB" . completion-at-point)))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package treemacs
	:defer t
	:config
	(progn
		(setq treemacs-show-hidden-files t
          treemacs-space-between-root-nodes nil
					treemacs-display-in-side-window t
					treemacs-position 'left
					treemacs-width 35)))

(use-package treemacs-all-the-icons
	:after (treemacs)
	:config
	(treemacs-load-theme 'all-the-icons))

(use-package lsp-ui
  :after (lsp-mode)
  :config
	(setq lsp-ui-doc-position 'at-point)
	(setq lsp-enable-snippet nil))

(use-package git-timemachine
	:config
	(with-eval-after-load 'git-timemachine
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

(use-package magit
	:config
	(setq magit-save-repository-buffers nil))

(use-package doom-modeline
	:init
	(setq doom-modeline-workspace-name nil)
	(setq doom-modeline-buffer-encoding nil)
	(setq doom-modeline-vcs-max-length 24)
	:hook (after-init . doom-modeline-mode))

(use-package solaire-mode
	:config
	(solaire-global-mode +1))

(use-package evil-leader
	:after evil
	:functions evil-leader/set-leader
	:config
	(global-evil-leader-mode)
	(add-to-list 'evil-buffer-regexps '("*Packages*" . normal)) ;; enable evil in packages-menu
	(evil-leader/set-leader ",")
	(evil-leader/set-key
	 "a" 'ace-window
	 "cp" 'copy-filepath-to-clipboard
	 "q" 'delete-window
	 "o" 'delete-other-windows
	 "r" 'lsp-find-references
	 "f" 'lsp-find-definition
	 "d" 'flycheck-list-errors
	 "s" 'yas-insert-snippet
	 "gl" 'magit-log-all
	 "gb" 'magit-show-commit
	 "gB" 'magit-blame
	 "gs" 'magit-status
	 "gc" 'magit-branch
	 "gh" 'magit-log-buffer-file
	 "gH" 'git-timemachine)
	(evil-mode t))

(use-package docker
	:bind ("C-c d" . docker))

(use-package go-mode
	:defines go-indent-level
	:hook ((go-mode . lsp-deferred))
	:config
	(setq gofmt-command "goimports")
	(setq go-indent-level 2)
	:hook (before-save-hook . gofmt-before-save))

(use-package typescript-mode
	:mode ("\\.ts\\'" "\\.tsx\\'")
	:hook (typescript-mode . lsp-deferred)
	:config
	(setq typescript-indent-level 2))

(use-package clojure-mode)

(use-package elpy
	:defer t
	:init
	(advice-add 'python-mode :before 'elpy-enable))

(use-package cider)

(use-package flycheck
	:init (global-flycheck-mode))

;(use-package tron-legacy-theme
;  :config
;	(setq tron-legacy-theme-softer-bg t)
;  (load-theme 'tron-legacy t))

(use-package doom-themes
	:defines doom-themes-enable-bolt
 	:config
 	(setq doom-themes-enable-bolt t
 				doom-themes-enable-italic t)
 	(load-theme 'doom-city-lights t)) ;; doom-nord  doom-wilmersdorf  doom-city-lights  doom-sourcerer  doom-outrun-electric  doom-vibrant

(use-package hideshow
	:defer t
  :diminish hs-minor-mode
  :hook (prog-mode  . hs-minor-mode))

(use-package yasnippet
	:functions yas-expand
  :diminish yas-minor-mode
  :preface (defvar tmp/company-point nil)
  :config
  (yas-global-mode +1)
  (advice-add 'company-complete-common
              :before
              #'(lambda ()
                  (setq tmp/company-point (point))))
  (advice-add 'company-complete-common
              :after
              #'(lambda ()
                  (when (equal tmp/company-point (point))
                    (yas-expand)))))
(use-package yasnippet-snippets)

(use-package company
	:config
	(setq company-minimum-prefix-length 1)
	(progn
            ;; don't add any dely before trying to complete thing being typed
            ;; the call/response to gopls is asynchronous so this should have little
            ;; to no affect on edit latency
            (setq company-idle-delay 0)
            ;; start completing after a single character instead of 3
            (setq company-minimum-prefix-length 1)
            ;; align fields in completions
            (setq company-tooltip-align-annotations t)))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package org
	:config
	(add-to-list 'org-emphasis-alist
             '("*" (:foreground "red")
               ))
	(setq org-link-frame-setup '((file . find-file-other-window)))
	(setq org-return-follows-link t)
	(setq org-pretty-entities t)
	(setq org-hide-emphasis-markers t)
	(setq org-startup-with-inline-images t)
	(setq org-startup-with-latex-preview t)
	(setq org-latex-create-formula-image-program 'dvisvgm)
	(setq org-latex-create-formula-image-program 'dvisvgm)
	(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
	(setq org-src-preserve-indentation t)
	(setq org-src-tab-acts-natively t)
	:bind (
				 :map org-mode-map
							("C-j" . windmove-down)
							("C-k" . windmove-up)
							("C-p" . org-roam-node-find)
							("C-f" . consult-ripgrep)
							("C-c n" . org-roam-capture)
							("C-'" . org-roam-buffer-toggle)
              ("C-c i" . org-roam-node-insert))
	:hook((prog-mode . yas-minor-mode)))

(use-package org-bullets
	:after org
	:init
	(setq org-bullets-bullet-list '("\u200b"))
	:hook (org-mode . org-bullets-mode))

(use-package org-roam
	:defines org-roam-v2-act org-roam-db-update-method
	:custom
	(org-roam-directory "~/Notes/org-roam-notes/")
	:init
	(setq org-roam-v2-act t)
	(setq org-roam-db-update-method 'immediate)
	(setq org-roam-dailies-directory "~/Notes/org-roam-daily")
	(setq org-roam-db-node-include-function
				(lambda()
					(not (member 'drill' (org-get-tags)))))
	(setq org-roam-capture-templates
				;; M-x describe-variable on:  org-roam-capture-templates
				'(
					("d" "default" plain "%?"
						:if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
															 "#+title: ${title}\n#+startup: showall inlineimages latexpreview\n#+tags: %^{org-roam-tags}\n#+created: %u\n#+options: ^:{}\n")
						:unnarrowed t)
					("c" "code snippet" plain "%?"
						:if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
															 "#+title: ${title}\n#+tags: %^{org-roam-tags}\n#+created: %u\n\n#+BEGIN_SRC\n\n#+END_SRC\n"))
					))
	(org-roam-setup))

(use-package org-download
	:after org
	:custom
	(org-download-method 'directory)
  (org-download-image-dir "images")
	(org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
	(org-image-actual-width 300)
	(org-download-screenshot-method "/usr/local/bin/pngpaste %s")
	:config
	(require 'org-download))

(use-package simple-httpd)

(use-package websocket)

(use-package org-roam-ui)

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

(use-package dashboard
	:config
	(setq dashboard-items '((projects . 5)
													(agenda . 5)))
	(dashboard-setup-startup-hook))

(use-package rainbow-delimiters)

(use-package marginalia
	:init
	(marginalia-mode))

(use-package consult
  :after selectrum
	:config
	(setq consult-project-root-function (lambda () (project-root (project-current))))) ;; consult for enhanced minibuffer commands

(defun find-with-ripgrep ()
	"Find stuff with ripgrep."
	(interactive)
	(message "executing find-with-ripgrep"))

(use-package embark
	:bind
	(("M-o" . embark-act))
	:init
	:config
	(eval-when-compile
  (defmacro my/embark-split-action (fn split-type)
    `(defun ,(intern (concat "my/embark-"
                             (symbol-name fn)
                             "-"
                             (car (last  (split-string
                                          (symbol-name split-type) "-"))))) ()
       (interactive)
       (funcall #',split-type)
       (call-interactively #',fn))))

	(define-key embark-file-map     (kbd "C-s") (my/embark-split-action find-file evil-window-split))
	(define-key embark-buffer-map   (kbd "C-s") (my/embark-split-action switch-to-buffer evil-window-split))
	(define-key embark-bookmark-map (kbd "C-s") (my/embark-split-action bookmark-jump evil-window-split))

	(define-key embark-file-map     (kbd "C-v") (my/embark-split-action find-file evil-window-vsplit))
	(define-key embark-buffer-map   (kbd "C-v") (my/embark-split-action switch-to-buffer evil-window-vsplit))
	(define-key embark-bookmark-map (kbd "C-v") (my/embark-split-action bookmark-jump evil-window-vsplit)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package ace-window)

(use-package helpful)

(use-package rg
	:config
	(setq rg-group-result t)
	(setq rg-hide-command t)
	(setq rg-show-columns nil)

	(rg-define-search robert/grep-vc-or-dir
		:query ask
		:format regexp
		:files "everything"
		:dir (let ((vc (vc-root-dir)))
					 (if vc
							 vc
						 default-directory))
		:confirm prefix
		:flags ("--hidden -g !.git"))
	:bind (("C-f" . robert/grep-vc-or-dir))
	)

(use-package olivetti
	:hook (
				 (org-mode . olivetti-mode)
				 (olivetti-mode-on-hook . (lambda () (olivetti-set-width 128))))
	:config
	(setq-default olivetti-body-width 128))

(use-package sqlformat
	:commands (sqlformat sqlformat-buffer sqlformat-region)
	:hook (sql-mode . sqlformat-on-save-mode)
	:init
	(setq sqlformat-command 'pgformatter
				sqlformat-args '("-s2" "-g" "-M" "-w100")))

(use-package undo-tree
	:init
	(global-undo-tree-mode))

(use-package tex
	:defer t
	:ensure auctex
	)

(use-package vterm)

(use-package multi-vterm)

(use-package projectile-rails)

(use-package rvm
	:config
	(rvm-use-default))

(use-package es-mode)

(use-package org-drill
	:init
	(setq org-drill-scope 'directory)
	(setq org-drill-add-random-noise-to-intervals-p t)
	(setq org-drill-hint-separator "||")
	(setq org-drill-left-close-delimiter "<[")
	(setq org-drill-right-close-delimiter "]>")
	(setq org-drill-learn-fraction 0.25))

(use-package pdf-tools
	:init
	(setq pdf-view-display-size 1.0)
	(pdf-tools-install))

(use-package pdf-view-restore
  :after pdf-tools
  :config
	(setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore")
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

;;(use-package kubel)

(use-package sly)

(use-package balanced-windows
  :config
  (balanced-windows-mode))

(use-package inf-ruby
	:hook (inf-ruby-switch-setup))

;; HELP FUCNTIONS ========================================================================================================================================================================================================================================
(defun new-named-tab ()
	"Create a new named tab."
	(interactive)
	(call-interactively 'tab-new)
	(tab-rename (read-string "Enter tab name: ")))

(global-set-key (kbd "s-t") 'new-named-tab)

(defun unique-shell ()
	"Create a new named shell buffer."
  (interactive)
  (call-interactively 'multi-vterm)
  (rename-buffer (read-string "Enter buffer name: ")))

(global-set-key (kbd "C-z") #'unique-shell)

(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

(defun robert/eshell-history (&optional initial-input)
	"Find command from eshell history. Initial-input can be given as the initial minibuffer input."
	(interactive)
	(insert
	  (completing-read "Find cmd: "
									 (robert/eshell-history-list))))

(defun robert/eshell-history-list ()
  "Return the eshell history as a list."
  (and (or (not (ring-p eshell-history-ring))
	   (ring-empty-p eshell-history-ring))
       (error "No history"))
  (let* ((index (1- (ring-length eshell-history-ring)))
	 (ref (- (ring-length eshell-history-ring) index))
	 (items (list)))
    (while (>= index 0)
      (setq items (cons (format "%s" (eshell-get-history index)) items)
	    index (1- index)
	    ref (1+ ref)))
    items))


(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

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

(add-hook 'eshell-mode-hook
          (lambda ()
						(define-key eshell-mode-map (kbd "C-h") #'robert/eshell-history)
            (define-key eshell-mode-map (kbd "C-n") #'treemacs)))

;; Try to use vterm-mode-map and keybind "C-h" to history completion function
;; have to investigate whehther vterm exposes its history


;; Insert at prompt only on eshell
(add-hook 'eshell-mode-hook
					'(lambda ()
						 (define-key evil-normal-state-local-map (kbd "i") (lambda () (interactive) (evil-goto-line) (evil-append-line nil)))))

(add-hook 'vterm-mode-hook
					'(lambda ()
						 (define-key evil-normal-state-local-map (kbd "i") (lambda () (interactive) (evil-goto-line) (evil-append-line nil)))))

(add-hook 'pdf-view-mode-hook
					(lambda ()
						(set (make-local-variable 'evil-normal-state-cursor) (list nil))
						(evil-set-initial-state 'pdf-view-mode 'normal)
						(pdf-view-midnight-minor-mode)))

(add-hook 'outline-mode-hook
					(lambda ()
						(evil-set-initial-state 'outline-mode 'normal)))

(defun evil-org-insert-state-in-edit-buffer (fun &rest args)
  "Bind `evil-default-state' to `insert' before calling FUN with ARGS."
  (let ((evil-default-state 'insert)
        ;; Force insert state
        evil-emacs-state-modes
        evil-normal-state-modes
        evil-motion-state-modes
        evil-visual-state-modes
        evil-operator-state-modes
        evil-replace-state-modes)
    (apply fun args)
    (evil-refresh-cursor)))

(advice-add 'org-babel-do-key-sequence-in-edit-buffer
            :around #'evil-org-insert-state-in-edit-buffer)

;; KEYBINDINGS ===========================================================================================================================================================================================================================================
(with-eval-after-load 'prog-mode (bind-key "C-'" #'lsp-ui-imenu))
(with-eval-after-load 'prog-mode (bind-key "C-f" #'consult-ripgrep))

(define-key package-menu-mode-map (kbd "C-h") 'evil-window-left)
(define-key package-menu-mode-map (kbd "C-j") 'evil-window-down)
(define-key package-menu-mode-map (kbd "C-k") 'evil-window-up)
(define-key package-menu-mode-map (kbd "C-l") 'evil-window-right)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

(define-key evil-normal-state-map (kbd "u") 'undo-tree-visualize)

(define-key evil-insert-state-map (kbd "s-k") 'comint-clear-buffer)

;;(define-key outline-mode-map (kbd "j") 'next-line)
;;(define-key outline-mode-map (kbd "k") 'previous-line)
;;(define-key outline-mode-map (kbd "C-l") 'evil-window-right)
;;(define-key outline-mode-map (kbd "C-n") 'pdf-outline-quit)

;; MODES =================================================================================================================================================================================================================================================

(show-paren-mode 1)

;; =======================================================================================================================================================================================================================================================
(evil-ex-define-cmd "q" 'kill-this-buffer)

(tool-bar-mode -1) ;; Disable Toolbar

(add-hook 'emacs-startup-hook 'toggle-frame-maximized) ;; Make fullscreen

(setq inhibit-splash-screen t) ;; Disable splash screen

(setq initial-major-mode (quote fundamental-mode)) ;; Disable scratch screen

(setq ring-bell-function 'ignore) ;; silence alert chimes

(global-font-lock-mode 1) ;; Enable syntax highlighting

(set-default 'truncate-lines t) ;; Don't wrap lines

(toggle-scroll-bar -1) ;; Don't show scroll bars

(global-linum-mode t) ;; Show number lines
(add-hook 'treemacs-mode-hook (lambda() (linum-mode 0)))
(add-hook 'vterm-mode-hook (lambda() (linum-mode 0)))
(add-hook 'org-mode-hook (lambda() (linum-mode 0)))
;;(add-hook 'org-mode-hook (lambda() (org-hide-properties)))
(add-hook 'sql-mode-hook 'sqlformat-on-save-mode)
(add-hook 'eshell-mode-hook (lambda()
															(define-key evil-normal-state-map (kbd "C-n") 'treemacs)
															(linum-mode 0)))

(add-hook 'sql-interactive-mode-hook
	  (lambda ()
	    (sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
	    (sql-set-product-feature 'postgres :prompt-cont-regexp
                           "^[-[:alnum:]_]*[-(][#>] ")))

(setq scroll-step 1
      scroll-conservatively  10000) ;; Smoother scrolling with smaller steps

(defadvice split-window (after move-point-to-new-window activate)
  "Move the point to the newly created window after splitting."
  (other-window 1)) ;; Focus cursor in new split buffers

(setq-default tab-width 2) ;; Set tab space to 2

(setq org-hidden-keywords '(title))

(set-face-bold-p 'bold nil)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-frame-font "Hack Nerd Font Mono" nil t)
(set-face-attribute 'default nil :height 100)

(setq-default explicit-shell-file-name "/bin/zsh")

(setq lsp-enable-links nil)

(setq show-paren-style 'parenthesis)

(cond
  ((string-equal system-type "darwin")
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))))
(setq frame-title-format '(""))

(set-cursor-color "#00FFFF")

(setq eshell-prompt-regexp "^[^λ]+ λ ")
(setq eshell-prompt-function
			(lambda ()
				(concat
				 (propertize (concat (eshell/pwd)) 'face `(:foreground "black" :background "turquoise1"))
				 (propertize "" 'face `(:foreground "turquoise1" :background "gray34"))
				 (if (magit-get-current-branch)
						 (concat
							(propertize "  " 'face `(:foreground "turquoise1" :background "gray34"))
							(propertize (magit-get-current-branch) 'face `(:foreground "turquoise1" :background "gray34"))
							(propertize "" 'face `(:foreground "gray34"))
						 ))
				 (propertize "\n")
         (propertize "❱ " 'face `(:foreground "white"))
				 )))

;; allows vim '*' to grab words with underscores:  i.e   match_this_whole_thing
(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t))

;; https://www.gnu.org/software/emacs/manual/html_node/eshell/Input_002fOutput.html#Input_002fOutput 
;;(add-to-list 'eshell-visual-commands "git")
;;(add-to-list 'eshell-visual-commands "rails")

;; remove duplicate commands from history
(setq comint-input-ignoredups t)

;; =======================================================================================================================================================================================================================================================
;;; init.el ends here
