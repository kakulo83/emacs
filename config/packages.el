;;; package --- Summary
;;; Commentary:
;;; Code:
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
  (setq evil-shift-width 2)
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
	:defines selectrum-mode
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
													(highlight-indentation-mode -1)
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
	:defines treemacs-load-theme
	:functions treemacs-load-theme
	:after (treemacs)
	:config
	(treemacs-load-theme 'all-the-icons))

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
	:defines evil-leader/set-leader
	:functions evil-leader/set-leader
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
	 "e" 'flycheck-list-errors
	 "s" 'yas-insert-snippet
	 "m" 'consult-man
	 "/" 'string-rectangle
	 "gl" 'magit-log-all
	 "gb" 'magit-show-commit
	 "gB" 'magit-blame
	 "gs" 'magit-status
	 "gc" 'magit-branch
	 "gh" 'magit-log-buffer-file
	 "gH" 'git-timemachine)
	(evil-mode t))

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
	:init (global-flycheck-mode)
	:config
	(setq flycheck-check-syntax-automatically '(save mode-enable))
	(add-to-list 'display-buffer-alist
								`(,(rx bos "*Flycheck errors*" eos)
								(display-buffer-reuse-window
								display-buffer-in-side-window)
								(side            . bottom)
								(reusable-frames . visible)
								(window-height   . 0.33))))

;(use-package tron-legacy-theme
;  :config
;	(setq tron-legacy-theme-softer-bg t)
;  (load-theme 'tron-legacy t))

;(use-package sublime-themes
;	; NOTE:  For granger theme I changed the "fringe" background color to "background"
;	;        to get rid of that annoying gray frame separator line
;	; https://github.com/owainlewis/emacs-color-themes
;	:config
;	(load-theme 'graham t)) ;; graham  fogus  granger

(use-package modus-operandi-theme
	:config
	(load-theme 'modus-vivendi t))  ;; modus-operandi    modus-vivendi

;(use-package afternoon-theme
;	:config
;	(load-theme 'afternoon t))

;(use-package doom-themes
;	:defines doom-themes-enable-bolt
; 	:config
; 	(setq doom-themes-enable-bolt t
; 				doom-themes-enable-italic t)
; 	(load-theme 'doom-city-lights t)) ;; doom-nord  doom-wilmersdorf  doom-city-lights  doom-sourcerer  doom-outrun-electric  doom-vibrant

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
            (setq company-tooltip-align-annotations t))
	:hook (prog-mode . company-mode))

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
	(setq org-preview-latex-default-process 'dvisvgm)
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
	:defines org-roam-v2-act org-roam-db-update-method org-roam-dailies-directory
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
					)))

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
	:defines consult-project-root-function
  :after selectrum
	:config
	(setq consult-project-root-function (lambda () (project-root (project-current))))) ;; consult for enhanced minibuffer commands

(defun find-with-ripgrep ()
	"Find stuff with ripgrep."
	(interactive)
	(message "executing find-with-ripgrep"))

(use-package embark
	:defines aw-dispatch-always embark-completing-read-prompter-map embark-completing-read-prompter
	:functions embark-completing-read-prompter-map with-minibuffer-keymap
	:bind
	(("M-o" . embark-act))
	:init
	:config

	;; Selecting commands via completions instead of key bindings
	;; (setq embark-prompter 'embark-completing-read-prompter)
	
	(eval-when-compile
  (defmacro my/embark-ace-action (fn)
    `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (require 'ace-window)
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn)))))))
  (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))

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

	(define-key embark-file-map     (kbd "C-s") (my/embark-split-action find-file split-window-below))
	(define-key embark-buffer-map   (kbd "C-s") (my/embark-split-action switch-to-buffer split-window-below))
	(define-key embark-bookmark-map (kbd "C-s") (my/embark-split-action bookmark-jump split-window-below))

	(define-key embark-file-map     (kbd "C-v") (my/embark-split-action find-file split-window-right))
	(define-key embark-buffer-map   (kbd "C-v") (my/embark-split-action switch-to-buffer split-window-right))
	(define-key embark-bookmark-map (kbd "C-v") (my/embark-split-action bookmark-jump split-window-right)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package ace-window
	:config
	(setq aw-dispatch-always t)
  (setq aw-dispatch-alist
	      '((?h aw-split-window-vert "Vertcal Split")
					(?v aw-split-window-horz "Horizontal Split")
				  (?e aw-switch-buffer-other-window "Switch Buffer Other Window")
					(?? aw-show-dispatch-help)
					)))

(use-package helpful)

(use-package olivetti
	:defines olivetti-set-width
	:functions olivetti-set-width
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
	:config
	(setq undo-tree-auto-save-history 0)
	:init
	(global-undo-tree-mode))

(use-package tex
	:defer t
	:ensure auctex
	)

(use-package vterm
	:config
	(setq vterm-max-scrollback 20000))

(use-package multi-vterm)

(use-package rvm
	:config
	(rvm-use-default))

(use-package es-mode)

(use-package org-drill
	:defines org-drill-hint-separator org-drill-left-close-delimiter org-drill-right-close-delimiter
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

(use-package sly)

(use-package balanced-windows
  :config
  (balanced-windows-mode))

(use-package inf-ruby
	:hook (inf-ruby-switch-setup))

(use-package restclient)

(use-package enh-ruby-mode
	:config
	(add-to-list 'load-path "(path-to)/Enhanced-Ruby-Mode") ; must be added after any path containing old ruby-mode
  (autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode)))

(use-package tree-sitter-langs)

(use-package tree-sitter
  :config
	(require 'tree-sitter-langs)
	(global-tree-sitter-mode)
	(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package yafolding
  :config
	(add-hook 'ruby-mode-hook 'yafolding-mode))

;;; packages.el ends here
