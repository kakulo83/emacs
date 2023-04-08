;;; package --- Summary
;;; Commentary:
;;; Code:

;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

(use-package bug-hunter)

(use-package projectile
	:functions projectile-project-root
	:init
	(setq projectile-globally-ignored-file-suffixes '("~undo-tree~"))
	(setq projectile-switch-project-action (lambda()
						 (projectile-dired)
						 (cd (projectile-project-root))))
	(projectile-mode 1))

(use-package evil
	:functions org-roam-capture org-roam-capture
  :init
	(setq evil-want-keybinding nil)
	:config
	(modify-syntax-entry ?_ "w")
  (setq evil-insert-state-cursor '(bar "#00FF00")
      evil-visual-state-cursor '(box "#FF00FF")
      evil-normal-state-cursor '(box "red"))
	(evil-mode)
  (setq evil-shift-width 2))

(use-package evil-collection
	:defines evil-collection-company-use-tng
	:after evil
	:config
	(setq evil-collection-mode-list
				'(vterm
					dired
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

(use-package lsp-mode
  :commands (lsp lsp-deferred)
	:config
	(setq lsp-lens-enable t
        lsp-enable-links nil
				lsp-signature-auto-activate nil
				lsp-keymap-prefix "C-c l"
				lsp-headerline-breadcrumb-enable nil)
	:hook (
				 (go-mode . lsp-deferred)
				 (sql-mode . lsp-deferred)
				 (typescript-mode . lsp-deferred)
				 (js-mode . lsp-deferred)))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

(use-package go-mode
	:config
	(setq gofmt-show-errors nil)
	(add-hook 'before-save-hook #'gofmt-before-save)
	(add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package gotest)

(use-package all-the-icons)

(use-package all-the-icons-completion
	:after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package all-the-icons-dired
	:hook
	(dired-mode . all-the-icons-dired-mode))

(use-package vertico
	:config
	(setq vertico-count 20)
	(setq vertico-resize nil)
	:init
	(vertico-mode))

(use-package vertico-posframe
	:straight (vertico-posframe :type git :host github :repo "tumashu/vertico-posframe")
	:init
	(vertico-posframe-mode 1))


(use-package orderless
	:init
	(setq completion-styles '(orderless basic)
				completion-category-defaults nil
				completion-category-overrides '((file (styles . (partial-completion))))))

(use-package magit
	:config
	(setq magit-save-repository-buffers nil))

(use-package git-timemachine
	:config
	(add-to-list 'evil-emacs-state-modes 'git-timemachine-mode))

(use-package nano-modeline
	:config
	(setq nano-modeline-position "bottom")
	(nano-modeline-mode))

; this package hides certain modes from cluttering the modeline
(use-package blackout
	:config
	(blackout 'js-mode)
	(blackout 'ruby-mode)
	(blackout 'python-mode)
	(blackout 'emacs-lisp-mode))

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

(use-package tron-legacy-theme
  :config
	(setq tron-legacy-theme-softer-bg t))

(use-package sublime-themes)

(use-package modus-themes)

(use-package planet-theme)

(use-package doom-themes
	:defines doom-themes-enable-bolt
 	:config
 	(setq doom-themes-enable-bolt t
 				doom-themes-enable-italic t))

(use-package nano-theme)

(use-package nord-theme)

(use-package hideshow
	:defer t
  :diminish hs-minor-mode
  :hook (prog-mode  . hs-minor-mode))

(use-package yasnippet
	:functions yas-expand
  :diminish yas-minor-mode
  :preface (defvar tmp/company-point nil)
  :config
  (setq yas-also-auto-indent-first-line t)
	(setq yas-indent-line 'auto)
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

(use-package org
	:config
	(add-to-list 'ibuffer-never-show-predicates "^\\*dired")
	(add-to-list 'org-emphasis-alist
             '("*" (:foreground "red")
               ))
	(setq org-link-frame-setup '((file . find-file))) ; find-file-other-window
	(setq org-return-follows-link t)
	(setq org-pretty-entities t)
	(setq org-hide-emphasis-markers t)
	(setq org-startup-with-inline-images t)
	(setq org-startup-indented t)
	;(setq org-startup-with-latex-preview t)
	(setq org-preview-latex-default-process 'dvisvgm)
	(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
	(setq org-src-preserve-indentation t)
	(setq org-src-tab-acts-natively t)
	(setq org-adapt-indentation t)
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
	:config
	(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))
;	(add-to-list 'display-buffer-alist
;             '("\\*org-roam\\*"
;               (display-buffer-in-side-window)
;               (side . right)
;               (slot . 0)
;               (window-width . 0.33)
;               (window-parameters . ((no-other-window . t)
;                                     (no-delete-other-windows . t)))))
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

(use-package org-roam-ui
        :config
	(setq org-roam-ui-follow t))

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

;(use-package org-modern-indent
;	:straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
;	:config
;	(add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package simple-httpd)

(use-package websocket)

(use-package dashboard
	:config
	(setq dashboard-items '((projects . 5) (agenda . 5))
				dashboard-set-file-icons t
				dashboard-image-banner-max-height 300 
				dashboard-startup-banner "~/Downloads/small-farm.png"
				dashboard-banner-logo-title "Buttercup was raised on a small farm in the country of Florin"
				)
	(dashboard-setup-startup-hook))

(use-package rainbow-delimiters)

(use-package marginalia
	:init
	(marginalia-mode))

(use-package consult
	:defines consult-project-root-function
  :after vertico
	:config
	(setq consult-project-root-function (lambda () (project-root (project-current))))) ;; consult for enhanced minibuffer commands

(use-package ace-window
	:config
	(setq aw-dispatch-always t)
  (setq aw-ignored-buffers '(" *Minibuf-1*"))
  (setq aw-dispatch-alist
				; NOTE:  triggering the ace action when the help window is open causes the action to
				; double the buffers, seems to be a bug.  If trigger the action without consulting the help
				; menu causes it to work perfectly
	      '((?s aw-split-window-vert "Vertcal Split")
					(?v aw-split-window-horz "Horizontal Split")
				  (?e aw-switch-buffer-other-window "Switch Buffer Other Window")
					(?m aw-move-window "Move Buffer")
					(?? aw-show-dispatch-help)
					))
	(ace-window-display-mode -1)
	:bind
	(("M-a" . 'ace-window)))

(use-package embark
	:defines embark-indicator embark-mixed-indicator aw-dispatch-always embark-completing-read-prompter-map embark-completing-read-prompter
	:functions embark-completing-read-prompter-map with-minibuffer-keymap
	:bind
	(("M-o" . embark-act))
	:config
  ;; NOTE:  embark shows UI in extended-mini-buffer
	;;        from customizing this:  embark-verbose-indicator-display-action
  (setq embark-indicator #'embark-mixed-indicator)
  (setq embark-verbose-indicator-display-action
   '(display-buffer-at-bottom
  	 (window-height . fit-window-to-buffer)))

	(eval-when-compile
		(defmacro my/embark-ace-action (fn)
				`(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
				(interactive)
				(with-demoted-errors "%s"
						(require 'ace-window)
						(let ((aw-dispatch-always t))
						(aw-switch-to-window (aw-select nil))
						(call-interactively (symbol-function ',fn)))))))

	(eval-when-compile
	(defun my/embark-find-org-roam-notes-for-identifier ()
		"Find any org roam notes pertaining to identifier on point."
		(message "searching notes for <identifier>")))

	(eval-when-compile
  (defmacro my/embark-split-action (fn split-type)
		"Split buffer and perform passed function"
    `(defun ,(intern (concat "my/embark-"
                             (symbol-name fn)
                             "-"
                             (car (last  (split-string
                                          (symbol-name split-type) "-"))))) ()
       (interactive)
       (funcall #',split-type)
       (call-interactively #',fn))))

	(defun my/embark-org-roam-cut-to-new-note (start end)
		"Cut region and populate new org-roam note."
		; capture content of current region and bind to variable
		; (delete-and-extract-region (region-beginning) (region-end))
		(interactive "r")
				(let* ((text (delete-and-extract-region start end))
							 (tags (read-string "Enter tags: "))
							 (title (read-string "Title of note: "))
							 (slug (org-roam-node-slug (org-roam-node-create :title title)))
							 (filename (format "%s/%d-%s.org"
	 														(expand-file-name org-roam-directory)
	 														(time-convert (current-time) 'integer)
	 														slug))
							 (org-id-overriding-file-name filename)
							 id)
	 			(with-temp-buffer
	 				(insert ":PROPERTIES:\n:ID:        \n:END:\n#+title: "
	 						title)
	 				(goto-char 25)
	 				(setq id (org-id-get-create))
					(goto-char (point-max))
					(insert "\n#+startup: showall inlineimages")
					(insert "\n#+tags:")
					(insert "\n#+options: ^:{}")
					(goto-char (point-max))
					(insert "\n\n")
					(goto-char (point-max))
					(insert text)
	 				(write-file filename)
	 				(org-roam-db-update-file filename)
	 				(format "[[id:%s][%s]]" id title))
				; insert link in place of moved text
				  (insert (concat "[[id:" id "][" title "]]"))
				))

	; EXPRESSION ACTIONS
	(define-key embark-expression-map "." #'my/embark-convert-to-python-path)

	; PERSPECTIVE ACTIONS
	(add-to-list 'marginalia-prompt-categories '("Perspective" . perspective))
	(defvar-keymap embark-perspective-keymap
		:doc "Keymap for perspective actions."
		:parent embark-file-map
		"k" #'persp-kill)
	(add-to-list 'embark-keymap-alist '(perspective . embark-perspective-keymap))

	; ORG-ROAM ACTIONS
	(add-to-list 'marginalia-prompt-categories '("OrgRoam" . org-roam))
	(defvar-keymap embark-org-roam-node-keymap
		:doc "Keymap for org-roam actions."
		:parent embark-file-map
		"o" (my/embark-ace-action org-roam-node-find))
	(add-to-list 'embark-keymap-alist '(org-roam-node . embark-org-roam-node-keymap))

	; REGION ACTIONS
	(define-key embark-region-map "c" #'my/embark-org-roam-cut-to-new-note)
	(define-key embark-region-map "f" #'fill-region)

	; ACE WINDOW ACTIONS
	(define-key embark-identifier-map (kbd "d") (my/embark-ace-action lsp-describe-thing-at-point))
	(define-key embark-identifier-map (kbd "o") (my/embark-ace-action lsp-find-definition))
	
  (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))

	(define-key embark-general-map (kbd "o") (my/embark-ace-action evil-lookup))
	)

(use-package embark-consult
  
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-dir
	:bind (("C-x C-d" . consult-dir)
				 :map minibuffer-local-completion-map
				 ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-yasnippet)

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
	(setq undo-tree-auto-save-history t)
	(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
	:init
	(global-undo-tree-mode))

(use-package tex
	:defer t
	:ensure auctex
	)

(use-package vterm
	:config
	(setq vterm-max-scrollback 20000))

(use-package multi-vterm
	:functions vterm-send-return evil-insert-state
  :config
  (add-hook 'vterm-mode-hook
  					(lambda ()
  						(setq-local evil-insert-state-cursor 'box)
  						(evil-insert-state)))
  (define-key vterm-mode-map [return]                      #'vterm-send-return)
  
  (setq vterm-keymap-exceptions nil)
  (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
  (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

(use-package org-drill
	:defines org-drill-hint-separator org-drill-left-close-delimiter org-drill-right-close-delimiter
	:init
	(setq org-drill-scope 'file)
	(setq org-drill-add-random-noise-to-intervals-p t)
	(setq org-drill-hint-separator "||")
	(setq org-drill-left-close-delimiter "<[")
	(setq org-drill-right-close-delimiter "]>")
	(setq org-drill-learn-fraction 0.25))

(use-package balanced-windows
  :config
  (balanced-windows-mode))

(use-package inf-ruby
	:hook (inf-ruby-switch-setup))

(use-package rspec-mode
	:config
	(setq rspec-use-bundler-when-possible nil))

(use-package restclient
	:config
	(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

; https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/
(use-package tree-sitter-langs
	:after tree-sitter)

(use-package tree-sitter
	:after tree-sitter-langs
  :config
	(require 'tree-sitter-langs)
	(global-tree-sitter-mode)
	(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package yafolding
	:after evil
  :config
	(add-hook 'ruby-mode-hook 'yafolding-mode))

; allows editing grep results and applying it to all files, good for global search/replace
(use-package wgrep)

(use-package web-mode
	:config
	(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)))

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(use-package python-pytest)

(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

(use-package perspective
	:custom
	(persp-mode-prefix-key (kbd "C-c M-p"))
	:config
	(setq persp-modestring-short t)
	:init
	(persp-mode))

(use-package persp-projectile
	:after (perspective)
	:straight (persp-projectile :host github :repo "bbatsov/persp-projectile"))

(use-package perspective-tabs
  :after (perspective)
  :straight (perspective-tabs :host sourcehut :repo "woozong/perspective-tabs")
	; http://www.gonsie.com/blorg/tab-bar.html
  ;	https://github.com/jimeh/.emacs.d/blob/c845af831690d1ab575b691020fbe91ce6435647/modules/workspaces/siren-tab-bar.el#L119-L138
  :init
	(defface robert-tab-bar-tab
    `((t :inherit 'tab-bar-tab
         :foreground "white",(face-attribute 'font-lock-keyword-face :foreground nil t)
				 ))
    "Face for active tab in tab-bar."
    :group 'robert-tab-bar)
  (defface robert-tab-bar-tab-hint
    `((t :inherit 'robert-tab-bar-tab
         :foreground ,(face-attribute 'tab-bar-tab-inactive :foreground nil t)))
    "Face for active tab hint in tab-bar."
    :group 'robert-tab-bar)

	(defface robert-tab-bar-tab-inactive
    `((t :inherit 'tab-bar-tab-inactive
				 :background ,(face-attribute 'font-lock-comment-face :background nil t)
         :foreground "slate gray",(face-attribute 'font-lock-comment-face :foreground nil t)))
    "Face for inactive tab in tab-bar."
    :group 'robert-tab-bar)
	(defface robert-tab-bar-tab-hint-inactive
    `((t :inherit 'robert-tab-bar-tab-inactive
         :foreground ,(face-attribute 'tab-bar-tab-inactive :foreground nil t)))
    "Face for inactive tab hint in tab-bar."
    :group 'robert-tab-bar)

	(defun robert-tab-bar-tab-format-function (tab i)
		(let* ((current-p (eq (car tab) 'current-tab))
           (tab-face (if current-p
                         'robert-tab-bar-tab
                       'robert-tab-bar-tab-inactive))
           (hint-face (if current-p
                          'robert-tab-bar-tab-hint
                        'robert-tab-bar-tab-hint-inactive)))
      (concat (propertize (if tab-bar-tab-hints (format "  %d:" (- i 1)) "  ")
                          'face hint-face)
              (propertize
               (concat
                (alist-get 'name tab)
                (or (and tab-bar-close-button-show
                         (not (eq tab-bar-close-button-show
                                  (if current-p 'non-selected 'selected)))
                         tab-bar-close-button)
                    "")
                "  ")
               'face tab-face))))
	(setq tab-bar-close-button-show nil)
	(setq tab-bar-new-button-show nil)
	(setq tab-bar-tab-name-format-function #'robert-tab-bar-tab-format-function)
  (perspective-tabs-mode +1))

(use-package lox-mode)

(use-package prettier-js
	:config
	(add-hook 'js-mode-hook 'prettier-js-mode))

(use-package dired-sidebar
	:config
	(setq dired-sidebar-recenter-cursor-on-follow-file nil)
	(setq dired-sidebar-should-follow-file nil)
	:custom
	(dired-subtree-line-prefix "  ")
	:bind
	(:map dired-sidebar-mode-map ("<return>" . 'dired-sidebar-find-file-alt)))

(use-package focus)

(use-package avy)

(use-package eshell
	:config
	(setq eshell-banner-message ""
			eshell-history-size 1000
			eshell-highlight-prompt t
			eshell-hist-ignoredups t))

(use-package eat
	:after eshell-mode
	:hook
	(eshell-load-hook . eat-eshell-mode)
	(eshell-load-hook . eat-eshell-visual-command-mode))

(use-package eshell-git-prompt
	:config
	(eshell-git-prompt-use-theme 'powerline))

(use-package eshell-syntax-highlighting
  :after eshell-mode
)

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  ;(eshell-toggle-init-function #'eshell-toggle-init-ansi-term)
  :bind
  ("s-`" . eshell-toggle))

;(use-package paredit
;	:init
;	(add-hook 'clojure-mode-hook #'enable-paredit-mode)
;  (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
;	(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
;	(add-hook 'ielm-mode-hook #'enable-paredit-mode)
;  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
;  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;  (add-hook 'scheme-mode-hook #'enable-paredit-mode))

(use-package arduino-mode)

(use-package platformio-mode)

(use-package corfu
	; https://kristofferbalintona.me/posts/202202270056/
	:hook (lsp-completion-mode . my/corfu-setup-lsp)
	:custom
	(corfu-auto t)
	(corfu-auto-delay 0)
	(corfu-auto-prefix 0)
	(completion-cycle-threshold nil)
	(corfu-quit-at-boundary nil)
  (corfu-separator ?\s) 
	(corfu-quit-no-match 'separator)
	(corfu-preview-current 'insert)
	(corfu-preselect-first t)
	(corfu-echo-documentation nil)
	(corfu-popupinfo-max-height 14)
	(lsp-completion-provider :none)
	:bind
	(:map corfu-map
				("C-n" . corfu-next)
				("C-p" . corfu-previous))
	:init
	(global-corfu-mode)
	(corfu-popupinfo-mode)
	:config
	(advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
	(defun my/corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf instead of the default lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
	)

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil) ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)

  ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
																				;(svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

  ;; Add hook to reset cache so the icon colors match my theme
  ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
  ;; the theme using my custom defined command for switching themes. If I don't
  ;; do this, then the backgound color will remain the same, meaning it will not
  ;; match the background color corresponding to the current theme. Important
  ;; since I have a light theme and dark theme I switch between. This has no
  ;; function unless you use something similar
  (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))

(use-package evil-leader
	:defines evil-leader/set-leader
	:functions evil-leader/set-leader
	:after evil perspective
	:functions evil-leader/set-leader persp-current-name
	:config
	(defun my-test-dispatch()
		"Run current test with respective test runner."
		(interactive)
		(if (eq major-mode 'go-mode)
				(go-test-current-test))
		(if (eq major-mode 'python-mode)
				(python-pytest-dispatch))
		(if (eq major-mode 'ruby-mode)
				(rspec-verify-single)))
	(defun my-persp-window-close()
  	(interactive)
		(if (= (length (window-list)) 1)
  			(call-interactively (persp-kill (persp-current-name)))
  		(delete-window)))
	(global-evil-leader-mode)
	(add-to-list 'evil-buffer-regexps '("*Packages*" . normal)) ;; enable evil in packages-menu
	(evil-leader/set-leader ",")
	(evil-leader/set-key
		"a" 'ace-window
		"cp" 'copy-filepath-to-clipboard
		"q"  'my-persp-window-close					; 'delete-window
		"o" 'delete-other-windows
		"e" 'flycheck-list-errors
		"s" 'consult-yasnippet
		"m" 'consult-man
		"/" 'string-rectangle
		"f" 'avy-goto-char-2
		"p" 'persp-switch
		"n" 'org-roam-node-find
		"t" 'my-test-dispatch
		"ya" 'yas-describe-tables
		"yn" 'yas-new-snippet
		"gl" 'magit-log-buffer-file
		"gL" 'magit-log-all
		"gb" 'magit-blame										; 'magit-show-commit
		"gs" 'magit-status
		"gc" 'magit-branch
		"gh" 'magit-log-buffer-file
		"gH" 'git-timemachine)
	(evil-mode t))
;;; packages.el ends here
