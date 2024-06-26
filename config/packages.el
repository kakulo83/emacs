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
  (setq evil-insert-state-cursor '(bar "#4682b4")
        evil-visual-state-cursor '(box "#b22222")
        evil-normal-state-cursor '(box "#32cd32"))
  (evil-mode)
  (setq evil-shift-width 2))

(use-package evil-collection
  :defines evil-collection-company-use-tng
  :after evil
  :config
  (setq evil-collection-mode-list
	'(vterm
	  occur
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

(use-package better-jumper
  :after evil
  :init
  (better-jumper-mode 1))
  ; https://github.com/Shopify/ruby-lsp/blob/main/EDITORS.md
  ; https://johnhame.link/posts/tweaking-emacs-for-ruby-development-in-2023/

(use-package eglot
  :ensure t
  :config
  ; https://www.reddit.com/r/emacs/comments/vau4x1/comment/ic6wd9i/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  ; tl;dr eglot writes events to an events-buffer that can become very large, parsing this is slow and causes eglot to slow emacs down alot
  (setq eglot-events-buffer-size 0)
  ;(with-eval-after-load 'eglot
  ;  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))
  :defer t
  :hook (
         (ruby-mode . eglot-ensure)
	 (python-ts-mode . eglot-ensure)
	 (go-mode . eglot-ensure)
	 (js-mode . eglot-ensure)
	 (typescript-ts-mode . eglot-ensure)
	 (sql-mode . eglot-ensure)))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :config 
  ; https://forum.shakacode.com/t/making-copilot-work-in-emacs-installation-and-keybindings/2491
  ; If you need to use Copilot behind a network proxy
  ; '(copilot-network-proxy
  ; '(:host <host-string> :port <port-number> :username <username-string> :password <password-string>))
  (defun robert/tab ()
    "Command to complete a copilot suggestion if available otherwise insert a tab."
    (interactive)
    (or (copilot-accept-completion)
        (indent-for-tab-command)))
  (define-key global-map (kbd "<tab>") #'robert/tab)
  (define-key copilot-mode-map (kbd "M-n") #'copilot-next-completion)
  (define-key copilot-mode-map (kbd "M-p") #'copilot-previous-completion)
  :hook
  (prog-mode . copilot-mode))

(use-package nyan-mode
  :config
  (setq nyan-animate-nyancat t)
  :init
  (nyan-mode 1))

(use-package doom-modeline
	:defines doom-modeline-mode-alist doom-modeline-support-imenu
	:functions doom-modeline-def-modeline
	:config
        (setq doom-modeline-hud t
              doom-modeline-persp-name nil
              doom-modeline-persp-icon nil
              doom-modeline-time-icon t
              doom-modeline-env-version nil
              doom-modeline-workspace-name nil
              doom-modeline-lsp nil
              doom-modeline-major-mode-icon nil
              doom-modeline-minor-modes nil
              doom-modeline-buffer-file-name-style 'relative-to-project
              doom-modeline-vcs-max-length 40
              doom-modeline-mode-alist nil
              doom-modeline-height 30
              doom-modeline-buffer-encoding nil
              doom-modeline-display-misc-in-all-mode-lines nil
              doom-modeline-position-line-format nil
              doom-modeline-percent-position nil
              doom-modeline-env-enable-ruby nil)
	:hook (after-init . doom-modeline-mode))

(use-package vertico
  :config
  (setq vertico-count 20)
  (setq vertico-resize nil)
  :init
  (vertico-mode))

;(use-package vertico-posframe
;  :init
;  (vertico-posframe-mode 1))

(use-package fussy
  :ensure t
  :config
  (push 'fussy completion-styles)
  (setq
   ;; For example, project-find-file uses 'project-files which uses
   ;; substring completion by default. Set to nil to make sure it's using
   ;; flx.
   completion-category-defaults nil
   completion-category-overrides nil))

(use-package orderless
  :init
  (setq completion-styles '(orderless flex)
        completion-category-overrides '((file (styles . (orderless flex))))))

(setq fussy-filter-fn 'fussy-filter-orderless)

(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters '(("Ruby" (rubocop"-a")))))

(use-package typescript-ts-mode
  :config
  (setq-default typescript-indent-level 4)
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-ts-mode)))

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


; https://www.reddit.com/r/emacs/comments/zqshfy/comment/j0zpwyo/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
(push '(css-mode . css-ts-mode) major-mode-remap-alist)
(push '(python-mode . python-ts-mode) major-mode-remap-alist)
(push '(javascript-mode . js-ts-mode) major-mode-remap-alist)
(push '(js-json-mode . json-ts-mode) major-mode-remap-alist)
(push '(typescript-mode . typescript-ts-mode) major-mode-remap-alist)

(use-package consult-eglot)

(use-package go-mode
  :config
  (setq gofmt-show-errors nil)
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :interpreter "ruby"
  :init
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil)
  (add-hook 'ruby-mode 'superword-mode))

(use-package rbenv
  :config
  (setq rbenv-show-active-ruby-in-modeline nil)
  (rbenv-use-corresponding))
  ;:init
  ;(global-rbenv-mode))

(use-package all-the-icons)

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package magit
  :config
  (setq magit-prefer-remote-upstream t)
  (setq magit-save-repository-buffers nil))

(use-package git-timemachine
  :config
  (add-to-list 'evil-emacs-state-modes 'git-timemachine-mode))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
        	 (display-buffer-reuse-window
        	  display-buffer-in-side-window)
        	 (side            . bottom)
        	 (reusable-frames . visible)
        	 (window-height   . 0.33))))

(use-package aggressive-indent)

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

(use-package gruvbox-theme)

(use-package badwolf-theme)

(use-package ef-themes)

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets")
        yas-indent-line 'auto)
  (yas-global-mode +1))

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
         ("C-c i" . org-roam-node-insert)))

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

;(use-package org-modern)

;(use-package org-modern-indent
;  :load-path "~/.emacs.d/straight/repos/org-modern-indent/"
;  :config
;  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

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
	  (?m aw-swap-window "Swap Buffer")
	  (?o delete-other-windows "Delete Other Windows")
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
  ;(setq embark-prompter 'embark-completing-read-prompter)
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
    (defmacro my/embark-perspective-action (fn)
      `(defun ,(intern (concat "my/embark-perspective-" (symbol-name fn))) ()
	 (interactive)
	 (with-demoted-errors "%s"
	   (require 'perspective)
           (persp-new "scratch")
           (tab-bar-select-tab-by-name "scratch")
           (call-interactively (symbol-function ',fn))
           (call-interactively 'tab-rename)
           ))))
  
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

                                        ; Page has list of different map types
                                        ; https://github.com/oantolin/embark/wiki/Default-Actions
  
                                        ; EXPRESSION ACTIONS
  (define-key embark-expression-map "." #'my/embark-convert-to-python-path)

                                        ; PERSPECTIVE ACTIONS
  (add-to-list 'marginalia-prompt-categories '("Perspective" . perspective))
  (defvar-keymap embark-perspective-keymap
    :doc "Keymap for perspective actions."
    :parent embark-file-map
    "k" #'persp-kill)
  (add-to-list 'embark-keymap-alist '(perspective . embark-perspective-keymap))

  (define-key embark-bookmark-map (kbd "p") (my/embark-perspective-action bookmark-jump))

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
  (define-key embark-identifier-map (kbd "o") (my/embark-ace-action xref-find-definitions))
  (define-key embark-identifier-map "n" #'eglot-rename)

  (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))

  (define-key embark-general-map (kbd "o") (my/embark-ace-action embark-dwim))
  )

(use-package embark-consult
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

(use-package json-reformat)

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

(use-package restclient
  :config
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

(use-package wgrep) ;; allows editing grep results and applying it to all files, good for global search/replace

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)))

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode))

(use-package nvm
  :straight (:host github :repo "rejeep/nvm.el"))

(use-package perspective
  :defer nil
  :demand t
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :config
  (setq persp-modestring-short nil
        persp-show-modestring nil
	persp-state-default-file "~/.emacs.d/.cache/saved-perspective-state"
	)
  (add-hook 'kill-emacs-hook #'persp-state-save)
  :init
  (persp-mode)
  (persp-turn-off-modestring))

(use-package persp-projectile
  :after (perspective)
  :straight (persp-projectile :host github :repo "bbatsov/persp-projectile"))

(use-package perspective-tabs
  :after (perspective)
  :straight (perspective-tabs :host sourcehut :repo "woozong/perspective-tabs")
  ; http://www.gonsie.com/blorg/tab-bar.html
  ; https://github.com/jimeh/.emacs.d/blob/c845af831690d1ab575b691020fbe91ce6435647/modules/workspaces/siren-tab-bar.el#L119-L138
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
  (setq dired-sidebar-should-follow-file t)
  (defun robert-dired-find-file ()
    "Like `find-file' but with `default-directory' set to the one specified by listing header."
    (interactive)
    (let ((default-directory (dired-current-directory)))
      (call-interactively #'find-file)))

  (with-eval-after-load 'evil
    (evil-define-key 'normal dired-mode-map
      (kbd "C-x C-f") 'robert-dired-find-file))
  :custom
  (dired-subtree-line-prefix "  ")
  :bind
  (:map dired-sidebar-mode-map ("<return>" . 'dired-sidebar-find-file-alt)))

(use-package focus)

(use-package avy)

(use-package eshell
  :config
  (setq eshell-prompt-function
     (lambda ()
       (concat
        (propertize "\n┌─ " 'face `(:foreground "royal blue"))
        (propertize (concat (eshell/pwd)) 'face `(:foreground "SteelBlue1"))
        (propertize " (" 'face `(:foreground "green"))
        (if (magit-get-current-branch)
            (propertize (magit-get-current-branch) 'face `(:foreground "green"))
            (propertize "z" 'face `(:foreground "yellow")))
        (propertize ")" 'face `(:foreground "green"))
        (propertize "\n" 'face `(:foreground "green"))
        (propertize "└─>" 'face `(:foreground "royal blue"))
        (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "royal blue"))
        )))
  (setq eshell-banner-message ""
	eshell-history-size 1000
	eshell-highlight-prompt t
        ; eshell-scroll-to-bottom-on-input t
	eshell-hist-ignoredups t))

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

(use-package evil-leader
  :defines evil-leader/set-leader
  :functions evil-leader/set-leader
  :after evil perspective
  :functions evil-leader/set-leader persp-current-name
  :config
  ;; https://emacs.stackexchange.com/questions/69672/change-and-later-restore-the-window-configuration-of-a-frame
  (defun my-save-windows-configuration-to-register()
    "Save the current window configuration to a register"
    (interactive)
    (window-configuration-to-register (read-string "Enter register: ")))
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
    "b" 'consult-bookmark
    "cc" 'recenter-top-bottom
    "cp" 'copy-filepath-to-clipboard
    "cf" 'focus-mode
    "q"  'my-persp-window-close	; 'delete-window
    "e" 'flycheck-list-errors
    "r" 'my-save-windows-configuration-to-register
    "R" 'consult-register
    "s" 'consult-yasnippet
    "m" 'consult-man
    "/" 'string-rectangle
    "f" 'avy-goto-char-2
    "p" 'persp-switch
    "n" 'org-roam-node-find
    "t" 'my-test-dispatch
    "y" 'consult-yank-from-kill-ring
    ;"ya" 'yas-describe-tables
    ;"yn" 'yas-new-snippet
    "gl" 'magit-log-buffer-file
    "gL" 'magit-log-all
    "gb" 'magit-blame ; 'magit-show-commit
    "gs" 'magit-status
    "gc" 'magit-branch
    "gh" 'magit-log-buffer-file
    "gp" 'magit-find-file
    "gH" 'git-timemachine)
  (evil-mode t))
;;; packages.el ends here
