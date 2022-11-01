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
  (setq evil-insert-state-cursor '(bar "#00FF00")
      evil-visual-state-cursor '(box "#FF00FF")
      evil-normal-state-cursor '(box "#E2E8EF"))
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
				 (sql-mode . lsp-deferred)
				 (js-mode . lsp-deferred)))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

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
	:hook
	(prog-mode . company-mode)
	(shell-mode . company-mode))

(use-package all-the-icons)

(use-package all-the-icons-dired
	:hook
	(dired-mode . all-the-icons-dired-mode))

(use-package vertico
	:config
	(setq vertico-count 20)
	(setq vertico-resize nil)
	:init
	(vertico-mode))

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

(use-package doom-modeline
	:defines doom-modeline-mode-alist doom-modeline-support-imenu
	:functions doom-modeline-def-modeline
	:config
	(setq doom-modeline-time-icon t)
	(setq doom-modeline-env-version nil)
	(setq doom-modeline-workspace-name nil)
	(setq doom-modeline-lsp nil)
	(setq doom-modeline-major-mode-icon nil)
	(setq doom-modeline-minor-modes nil)
	(setq doom-modeline-buffer-file-name-style 'relative-to-project)
	(setq doom-modeline-vcs-max-length 40)
	(setq doom-modeline-mode-alist nil)
	(setq doom-modeline-height 30)
	(setq doom-modeline-buffer-encoding nil)
	(setq doom-modeline-display-misc-in-all-mode-lines nil)
	:hook (after-init . doom-modeline-mode))

; this package hides certain modes from cluttering the modeline
(use-package blackout
	:config
	(blackout 'js-mode)
	(blackout 'ruby-mode)
	(blackout 'python-mode)
	(blackout 'emacs-lisp-mode))

(use-package evil-leader
	:defines evil-leader/set-leader
	:functions evil-leader/set-leader
	:after evil perspective
	:functions evil-leader/set-leader persp-current-name
	:config
	;; Tab Related
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
	 "q"  'my-persp-window-close ; 'delete-window
	 "o" 'delete-other-windows
	 "e" 'flycheck-list-errors
	 "s" 'yas-insert-snippet
	 "m" 'consult-man
	 "/" 'string-rectangle
	 "f" 'avy-goto-char-2
   "p" 'persp-switch
	 "ya" 'yas-describe-tables
	 "yn" 'yas-new-snippet
	 "gl" 'magit-log-buffer-file
	 "gL" 'magit-log-all
	 "gb" 'magit-blame ; 'magit-show-commit
	 "gs" 'magit-status
	 "gc" 'magit-branch
	 "gh" 'magit-log-buffer-file
	 "gH" 'git-timemachine)
	(evil-mode t))

(use-package go-mode
	:defines go-indent-level
	:config
	(setq gofmt-command "goimports")
	(setq go-indent-level 2)
	:hook (before-save-hook . gofmt-before-save))

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

(use-package iceberg-theme
	:config
	(iceberg-theme-create-theme-file))

(use-package doom-themes
	:defines doom-themes-enable-bolt
 	:config
 	(setq doom-themes-enable-bolt t
 				doom-themes-enable-italic t))

(use-package nano-theme)


(use-package hideshow
	:defer t
  :diminish hs-minor-mode
  :hook (prog-mode  . hs-minor-mode))

(use-package yasnippet
	:functions yas-expand
  :diminish yas-minor-mode
  :preface (defvar tmp/company-point nil)
  :config
	(setq yas-indent-line 'fixed)
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
	(add-to-list 'org-emphasis-alist
             '("*" (:foreground "red")
               ))
	(setq org-link-frame-setup '((file . find-file-other-window)))
	(setq org-return-follows-link t)
	(setq org-pretty-entities t)
	(setq org-hide-emphasis-markers t)
	(setq org-startup-with-inline-images t)
	;(setq org-startup-with-latex-preview t)
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

(use-package simple-httpd)

(use-package websocket)

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
  :after vertico
	:config
	(setq consult-project-root-function (lambda () (project-root (project-current))))) ;; consult for enhanced minibuffer commands

(use-package embark
	:defines aw-dispatch-always embark-completing-read-prompter-map embark-completing-read-prompter
	:functions embark-completing-read-prompter-map with-minibuffer-keymap
	:bind
	(("M-o" . embark-act))
	:init
	:config
  ;; NOTE:  embark shows UI in extended-mini-buffer
	;;        from customizing this:  embark-verbose-indicator-display-action

;  (setq embark-verbose-indicator-display-action
;   '(display-buffer-at-bottom
;  	 (window-height . fit-window-to-buffer)))
;
;	(setq embark-verbose-indicator-display-action
;				`((display-buffer-below-selected display-buffer-in-side-window)
;					(side . bottom)
;					(slot . 0)
;					(window-parameters
;					 (mode-line-format
;						. ,(propertize embark--verbose-indicator-buffer
;													 'face 'bold)))
;					(window-height . (lambda (win) (fit-window-to-buffer win (floor (frame-height) 3))))))
	
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
		"Split buffer and perform passed function"
    `(defun ,(intern (concat "my/embark-"
                             (symbol-name fn)
                             "-"
                             (car (last  (split-string
                                          (symbol-name split-type) "-"))))) ()
       (interactive)
       (funcall #',split-type)
       (call-interactively #',fn))))

	(add-to-list 'marginalia-prompt-categories '("consult-ripgrep" . consult-ripgrep))

	;(embark-define-keymap embark-consult-ripgrep-actions
	;	"Keymap for consult-ripgrep (when mentioned by name)."
  ;  ((kbd "C-v") (my/embark-split-action consult-ripgrep split-window-right))
  ;  ((kbd "C-s") (my/embark-split-action consult-ripgrep split-window-below)))

	;(add-to-list 'embark-keymap-alist '(consult-ripgrep . embark-consult-ripgrep-actions))
	
	;(define-key embark-consult-grep-map (kbd "C-v") (my/embark-split-action consult-ripgrep split-window-right))
	;(define-key embark-consult-grep-map (kbd "C-s") (my/embark-split-action consult-ripgrep split-window-below))

	(define-key embark-identifier-map (kbd "C-v") (my/embark-split-action lsp-find-definition split-window-right))
	(define-key embark-identifier-map (kbd "C-s") (my/embark-split-action lsp-find-definition split-window-below))

	(define-key embark-region-map "f" #'fill-region)
	
	(define-key embark-file-map     (kbd "C-s") (my/embark-split-action find-file split-window-below))
	(define-key embark-buffer-map   (kbd "C-s") (my/embark-split-action switch-to-buffer split-window-below))
	(define-key embark-bookmark-map (kbd "C-s") (my/embark-split-action bookmark-jump split-window-below))

	(define-key embark-file-map     (kbd "C-v") (my/embark-split-action find-file split-window-right))
	(define-key embark-buffer-map   (kbd "C-v") (my/embark-split-action switch-to-buffer split-window-right))
	(define-key embark-bookmark-map (kbd "C-v") (my/embark-split-action bookmark-jump split-window-right)))

(use-package embark-consult
  
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
				; NOTE:  triggering the ace action when the help window is open causes the action to
				; double the buffers, seems to be a bug.  If trigger the action without consulting the help
				; menu causes it to work perfectly
	      '((?s aw-split-window-vert "Vertcal Split")
					(?v aw-split-window-horz "Horizontal Split")
				  (?e aw-switch-buffer-other-window "Switch Buffer Other Window")
					(?m aw-move-window "Move Buffer")
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
	(setq undo-tree-auto-save-history nil)
	:init
	(global-undo-tree-mode))

(use-package tex
	:defer t
	:ensure auctex
	)

(use-package vterm
	:config
	(setq vterm-max-scrollback 20000)
	;(advice-add #'vterm--redraw :around (lambda (fun &rest args) (let ((cursor-type cursor-type)) (apply fun args))))
	(defun get-full-list ()
    (let ((program-list (split-string (shell-command-to-string "compgen -c") "\n" t ))
          (file-directory-list (split-string (shell-command-to-string "compgen -f") "\n" t ))
          (history-list (with-temp-buffer
                          (insert-file-contents "~/.zsh_history")
                          (split-string (buffer-string) "\n" t))))
			(delete-dups (append program-list file-directory-list history-list))))
	 (defun vterm-completion-choose-item ()
    (completing-read "Choose: " (get-full-list) nil nil (thing-at-point 'word 'no-properties)))

  (defun vterm-completion ()
    (interactive)
    ;(vterm-directory-sync)
    (let ((vterm-chosen-item (vterm-completion-choose-item)))
      (when (thing-at-point 'word)
        (vterm-send-meta-backspace))
      (vterm-send-string vterm-chosen-item)))
             
  (defun vterm-directory-sync ()
  "Synchronize current working directory."
  (interactive)
  (when vterm--process
    (let* ((pid (process-id vterm--process))
           (dir (file-truename (format "/proc/%d/cwd/" pid))))
      (setq default-directory dir))))

	:bind
	(:map vterm-mode-map ("<tab>" . 'vterm-completion)))

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
	(setq org-drill-scope 'directory)
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
(require 'wgrep)

(use-package web-mode
	:config
	(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)))

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

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

(use-package lox-mode)

(use-package prettier-js
	:config
	(add-hook 'js-mode-hook 'prettier-js-mode))

(use-package dired-sidebar
	:config
	(setq dired-sidebar-recenter-cursor-on-follow-file t)
	(setq dired-sidebar-should-follow-file t)
	:bind
	(:map dired-sidebar-mode-map ("<return>" . 'dired-sidebar-find-file-alt)))

(use-package focus)

(use-package avy)

;;; packages.el ends here
