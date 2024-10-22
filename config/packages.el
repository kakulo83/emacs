;;; package --- Summary -*- lexical-binding: t;-*-
;;; Commentary:
;;; Code:

;; :init executes code BEFORE a package is loaded
;; :config executes code AFTER a package is loaded

(use-package bug-hunter)


(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("LIBRARY_PATH" "INFOPATH" "CPATH" "MANPATH")))


(use-package evil
  :init
  :config
  (evil-set-undo-system 'undo-redo)
  (evil-set-initial-state 'package-menu-mode 'motion)
  (evil-mode 1))


(use-package evil-collection
  :defines evil-collection-company-use-tng
  :after evil
  :config
  (setq evil-collection-mode-list
	'(vterm
	  occur
	  ; https://github.com/emacs-evil/evil-collection/issues/100
	  ;(occur ,(if (<= emacs-major-version 25) "replace" 'replace))
	  ibuffer
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


(use-package doom-modeline
  :defines doom-modeline-mode-alist doom-modeline-support-imenu
  :functions doom-modeline-def-modeline
  :config
  (setq doom-modeline-hud t
    doom-modeline-modal nil
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
    doom-modeline-height 16
    doom-modeline-buffer-encoding nil
    doom-modeline-display-misc-in-all-mode-lines nil
    doom-modeline-percent-position nil
    doom-modeline-env-enable-ruby nil
    doom-modeline-env-version nil)
  :hook (after-init . doom-modeline-mode))


(use-package nyan-mode
  :after doom-modeline
  :config
  (nyan-mode 1)
  (nyan-start-animation))


(use-package magit)


(use-package git-timemachine
  :config
  (add-to-list 'evil-emacs-state-modes 'git-timemachine-mode))


(use-package dired-sidebar
  :init
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
  ;(add-hook 'dired-sidebar-mode-hook
  ;  (lambda ()
  ;    (unless (file-remote-p default-directory)
  ;	(auto-revert-mode))))
  :custom
  (dired-subtree-line-prefix "  ")
  :bind
  (:map dired-sidebar-mode-map ("<return>" . 'dired-sidebar-find-file-alt)))


(use-package diredfl
  :hook
  (dired-mode . diredfl-global-mode))


(use-package ace-window
  :init
  (setq aw-dispatch-always t)
  (setq aw-ignored-buffers '(" *Minibuf-1*"))
  ;; NOTE:  triggering the ace action when the help window is open causes the action to
  ;; double the buffers, seems to be a bug.  If trigger the action without consulting the help
  ;; menu causes it to work perfectly
  (setq aw-dispatch-alist
  	'((?s aw-split-window-vert "Vertcal Split")
  	  (?v aw-split-window-horz "Horizontal Split")
  	  (?e aw-switch-buffer-other-window "Switch Buffer Other Window")
  	  (?m aw-swap-window "Swap Buffer")
  	  (?o delete-other-windows "Delete Other Windows")
	  (?x aw-delete-window "Delete Window")
  	  (?? aw-show-dispatch-help)
  	  )))


(use-package balanced-windows
  :config
  (balanced-windows-mode))


;(use-package doom-themes
;  :config
;  (load-theme 'doom-tron t))  ; doom-acario-light  doom-nord-light   doom-city-lights   doom-outrun-electric   doom-wilmersdorf  doom-tron   doom-material    doom-manegarm
;(use-package ef-themes
;  :config
;  (load-theme 'ef-night t)) ; ef-duo-dark  ef-deuteranopia-light  ef-deuteranopia-dark  ef-maris-light   ef-elea-light  ef-winter   ef-night   ef-cherie
(use-package modus-themes
  :config
  (defun customize-modus ()
    (if (member 'modus-vivendi custom-enabled-themes)
      (custom-theme-set-faces
	'modus-vivendi
        '(fringe ((t (:background "black" :foreground "#ffffff")))))))
  (add-hook 'modus-themes-after-load-theme-hook 'customize-modus)
  (load-theme 'modus-vivendi t)) ; modus-operandi  modus-vivendi
;(use-package nano-theme
;  :config
;  (load-theme 'nano-dark t)) ; nano-light  nano-dark
;(use-package catppuccin-theme
;  :config
;  (setq catppuccin-flavor 'latte)  ; latte mocha macchiato frappe
;  (load-theme 'catppuccin t))


(use-package tabspaces
  :hook
  (after-init . tabspaces-mode)
  (after-init . tabspaces-reset-buffer-list)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-session nil)
  (tabspaces-session-auto-restore nil)
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-remove-to-default t)
  :config

  ;; Filter buffers for Consult-Buffer
  (with-eval-after-load 'consult
    ;; hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; set consult-workspace buffer list
    (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
	    :narrow   ?w
	    :history  'buffer-name-history
	    :category 'buffer
	    :state    #'consult--buffer-state
	    :default  t
	    :items    (lambda () (consult--buffer-query
			    :predicate #'tabspaces--local-buffer-p
			    :sort 'visibility
			    :as #'buffer-name)))

    "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace))
  ; faces are customized here to allow tabspaces to setup its state before we apply our customizations
  (setq tab-bar-new-button-show nil)
  (set-face-attribute 'tab-bar nil :foreground "grey" :background 'unspecified)
  (set-face-attribute 'tab-bar-tab nil :foreground "orange")
  (set-face-attribute 'tab-bar-tab-inactive nil :foreground 'unspecified :background 'unspecified :box nil)
  (set-face-attribute 'tab-bar-tab-group-inactive nil :foreground 'unspecified :background 'unspecified :box nil)
  )


(use-package corfu
  ;; corfu provides in-buffer completion with a small completion popup
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0)
  (corfu-quit-no-match 'separator)
  :bind
  (:map corfu-map
    ("M-n" . corfu-next)
    ("M-p" . corfu-previous))
  :config
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  (global-corfu-mode)
  (corfu-popupinfo-mode))


(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (setq corfu-popupinfo-min-height 50)
  (setq corfu-popupinfo-max-height 50)
  (corfu-popupinfo-mode))


(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(use-package vertico
  ;; vertico provides mini-buffer completion
  :config
  (setq vertico-count 20)
  (setq vertico-resize nil)
  :init
  (vertico-mode))


(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles partial-completion)))))


(use-package helpful)


(use-package embark
  :init
  ;; NOTE:  embark shows UI in extended-mini-buffer from customizing the variable
  ;;        embark-verbose-indicator-display-action
  ;(setq embark-prompter 'embark-completing-read-prompter)
  ;; NOTE:  show all available actions after a target is set with <C-h>
  (setq embark-indicators
      '(embark-minimal-indicator  ; default is embark-mixed-indicator
        embark-highlight-indicator
        embark-isearch-highlight-indicator))
  (setq display-buffer-alist '(("\\*Embark Export: .*" (display-buffer-reuse-mode-window display-buffer-same-window))))
  (setq embark-indicator #'embark-mixed-indicator)
  (setq embark-verbose-indicator-display-action
        '(display-buffer-at-bottom
  	  (window-height . fit-window-to-buffer)))
  :bind
  (("M-o" . embark-act)))


(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package wgrep)


(use-package consult
  :defines consult-project-root-function
  :after vertico
  :config
  (setq consult-project-root-function (lambda () (project-root (project-current)))))


(use-package marginalia
  :init
  (marginalia-mode))


(use-package symbols-outline
  :config
  (setq symbols-outline-fetch-fn #'symbols-outline-lsp-fetch)
  (setq symbols-outline-window-position 'right)
  ;(setq symbols-outline-use-nerd-icon-in-gui t)
  :init
  (symbols-outline-follow-mode))

(use-package all-the-icons)


(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))


(use-package all-the-icons-ibuffer
	:hook
	(ibuffer-mode . all-the-icons-ibuffer-mode))


(use-package gcmh
  :ensure t
  :config
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (add-hook 'after-init-hook (lambda ()
                               (gcmh-mode))))


(use-package eglot
  ; Documentation:
  ; configuring eglot: https://www.gnu.org/software/emacs/manual/html_mono/eglot.html#Project_002dspecific-configuration
  ; 
  ; M-x eglot-workspace-configuration
  :config
  ; https://www.reddit.com/r/emacs/comments/vau4x1/comment/ic6wd9i/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  ; Eglot writes events to an events-buffer that can become very large thus slowing emacs down
  ;(add-to-list 'eglot-server-programs '(elixir-ts-mode "/Users/robertcarter/Developer/elixir/elixir-ls-v0.24.1/language_server.sh"))
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "/opt/homebrew/bin/elixir-ls"))

  ;; setting language specific lsp configs
  (setq-default eglot-workspace-configuration
    '((elixir-ls
	(elixirLS.autoInsertRequiredAlias . nil))))
  (setq eglot-events-buffer-size 0)
  :defer t
  :hook (
	  (ruby-mode . eglot-ensure)
	  (elixir-ts-mode . eglot-ensure)
	  (go-mode . eglot-ensure)
	  (js-mode . eglot-ensure)
	  (typescript-ts-mode . eglot-ensure)
	  (sql-mode . eglot-ensure)))

;; ignore jsonrpc events to speed up eglot
(fset #'jsonrpc--log-event #'ignore)

(with-eval-after-load 'eglot
  (setq completion-category-defaults nil))


(use-package dape
  ;;https://github.com/svaante/dape?tab=readme-ov-file#configuration
  )


(use-package eshell
  :config
  (require 'em-smart)
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
        (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "DeepSkyBlue1"))  ;; prompt color
        )))
  (setq eshell-banner-message ""
    eshell-smart-space-goes-to-end t
    eshell-history-size 1000
    eshell-highlight-prompt t
    eshell-scroll-to-bottom-on-input t
    eshell-hist-ignoredups t))


(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-git-root t)
  (eshell-toggle-run-command nil)
  ;(eshell-toggle-init-function #'eshell-toggle-init-ansi-term)
  :bind
  ("s-`" . eshell-toggle))


(use-package vterm
  :config
  (setq vterm-max-scrollback 20000)
  (defun get-full-list ()
    (let ((history-list (with-temp-buffer
                          (insert-file-contents "~/.zsh_history")
                          (split-string (buffer-string) "\n" t))))

      (delete-dups (append history-list))))

  (defun vterm-completion-choose-item ()
    (completing-read "History: " (get-full-list) nil nil (thing-at-point 'word 'no-properties)))

  (defun vterm-completion ()
    (interactive)
    (setq vterm-chosen-item (vterm-completion-choose-item))
    (when (thing-at-point 'word)
      (vterm-send-meta-backspace))
    (vterm-send-string vterm-chosen-item)))


(use-package multi-vterm
  :functions vterm-send-return evil-insert-state
  :config
  (add-hook 'vterm-mode-hook
  	    (lambda ()
  	      (setq-local evil-insert-state-cursor 'box)
  	      (evil-insert-state))))


(use-package vterm-toggle
  :config
  (add-to-list 'display-buffer-alist
             '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
               (display-buffer-reuse-window display-buffer-in-side-window)
               (side . bottom)
               (reusable-frames . visible)
               (window-height . 0.5)))
  (setq vterm-toggle-fullscreen-p nil
        vterm-toggle-project-root t
        vterm-toggle-scope 'project))


(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
	:rev :newest
	:branch "main")
  :config
  (define-key copilot-mode-map (kbd "M-C-n") #'copilot-next-completion)
  (define-key copilot-mode-map (kbd "M-C-p") #'copilot-previous-completion)
  (define-key copilot-mode-map (kbd "M-C-l") #'copilot-accept-completion-by-word))


(use-package copilot-chat
  :vc (:url "https://github.com/chep/copilot-chat.el"
	:rev :newest
	:branch "master")
  :custom
  (copilot-chat-frontend 'org))


(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
  :init
  (global-undo-tree-mode))


(use-package avy)


(use-package org
  :config
  (setq org-emphasis-alist
    '(("*" (bold :foreground "Red" ))
       ("/" (italic :foreground "Orange"))
       ("_" underline)
       ("=" (:background "maroon" :foreground "white"))
       ("~" (:background "deep sky blue" :foreground "MidnightBlue"))
       ("+" (:strike-through t))))
  (setq org-link-frame-setup '((file . find-file))) ; find-file-other-window
  (setq org-return-follows-link t)
  (setq org-pretty-entities t)
  (setq org-hide-emphasis-markers t)
  (setq org-hidden-keywords '(title))
  (setq org-startup-with-inline-images t)
  ;(setq org-startup-with-latex-preview t)
  ;(setq org-preview-latex-default-process 'dvisvgm)
  ;(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq org-startup-indented t)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-adapt-indentation t)
  (let* ((variable-tuple
          (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

     (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 2.75 ))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 4.0 :underline nil))))))

  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin))))
   '(fixed-pitch ((t ( :family "Fira Code Retina" :height 160)))))

  (custom-theme-set-faces
   'user
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "deep sky blue" :underline t))))
   '(org-meta-line ((t (:inherit fixed-pitch :height 0.8))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

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
  (custom-set-variables '(org-bullets-bullet-list (quote ("◉" "○" "✸" "◉🌿"))))
  ;(custom-set-variables '(org-bullets-bullet-list (quote ("🌺" "🌸" "🌼" "🌿" "🍀" ))))
  ;(setq org-bullets-bullet-list '("\u200b")) ; for a blank bullet (hiding them)
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
		     "#+title: ${title}
# ------------------------------------------------------------------------------------------------------------------------------
#+startup: showall inlineimages
#+tags: %^{org-roam-tags}
#+created: %u
#+options: ^:{}
# ------------------------------------------------------------------------------------------------------------------------------
")
	    :unnarrowed t))))


(use-package org-roam-ui
  :config
  (setq org-roam-ui-follow t))


(use-package org-drill
  :defines org-drill-hint-separator org-drill-left-close-delimiter org-drill-right-close-delimiter
  :init
  (setq org-drill-scope 'file)
  (setq org-drill-add-random-noise-to-intervals-p t)
  (setq org-drill-hint-separator "||")
  (setq org-drill-left-close-delimiter "<[")
  (setq org-drill-right-close-delimiter "]>")
  (setq org-drill-learn-fraction 0.25))


(use-package org-download
  :after org
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-image-actual-width 300)
  (org-download-screenshot-method "/opt/homebrew/bin/pngpaste %s")
  :config
  (require 'org-download))


(use-package simple-httpd)


(use-package websocket)


(use-package olivetti
  :defines olivetti-set-width
  :functions olivetti-set-width
  :hook (
	 (org-mode . olivetti-mode)
	 (olivetti-mode-on-hook . (lambda () (olivetti-set-width 128))))
  :config
  (setq-default olivetti-body-width 128))


(use-package restclient
  :config
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))


(use-package nvm)


(use-package hydra)

    
(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
    '(("Python" (black))
       ("Go"    (gofmt) (goimports))
       ("JS"    (prettier))
       ("TS"    (prettier))
       ("HTML"  (prettier))
       ("CSS"   (prettier))
       ("JSON"  (prettier))
       ("YAML"  (prettier))
       ("XML"   (prettier))
       ("SQL"   (sqlformat)))))
                  
(use-package hide-mode-line
	:hook
	((completion-list-mode . hide-mode-line-mode)
	 (help-mode . hide-mode-line-mode)
	 (compilation-mode . hide-mode-line-mode)
	 (vterm-mode . hide-mode-line-mode)
	 (eshell-mode . hide-mode-line-mode)
	 (shell-mode . hide-mode-line-mode)
	 (term-mode . hide-mode-line-mode)
	 (dired-sidebar)))

(use-package typescript-ts-mode
  :config
  ;(setq-default typescript-indent-level 4)
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-ts-mode)))


(use-package pyvenv
	:config
	(pyvenv-mode 1))


(use-package disaster
  :init
  ;; If you prefer viewing assembly code in `nasm-mode` instead of `asm-mode`
  (setq disaster-assembly-mode 'asm-mode))


(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs
    '("~/.emacs.d/snippets")
    yas-indent-line 'auto)
  (yas-global-mode +1))


(use-package yasnippet-snippets
  :after yasnippet)

(use-package inf-ruby)


(use-package nodejs-repl)


(use-package inf-elixir
  :after consult
  :bind (("C-c i i" . 'inf-elixir)
	  ("C-c i p" . 'inf-elixir-project)
	  ("C-c i l" . 'inf-elixir-send-line)
          ("C-c C-c" . 'inf-elixir-send-region)
          ("C-c i b" . 'inf-elixir-send-buffer)
          ("C-c i R" . 'inf-elixir-reload-module)))
(add-hook 'inferior-elixir-mode-hook
  (lambda ()
    (setq-local corfu-auto nil)
    (corfu-mode)))

;; https://elixirforum.com/t/emacs-elixir-setup-configuration-wiki/19196/5
(use-package elixir-ts-mode
  :hook (elixir-ts-mode . eglot-ensure)
  (before-save . eglot-format)
  :config
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode)))


(use-package heex-ts-mode)


(use-package exunit
  :config
  (add-hook 'elixir-mode-hook 'exunit-mode))


(use-package highlight-symbol)


(use-package flycheck
  :ensure t
  :config
  (setq flycheck-check-syntax-automatically '(save new-line))
  (setq flycheck-display-errors-delay 0.2)
  (setq flycheck-highlighting-mode 'symbol)
  (setq flycheck-indication-mode 'left-fringe)
  (add-to-list 'display-buffer-alist
    `(,(rx bos "*Flycheck errors*" eos)
       (display-buffer-in-side-window)
       (side . bottom)
       (reusable-frames . visible)
       (window-height . 0.33)))
  :init (global-flycheck-mode))


(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))


(provide 'packages)

;;; packages.el ends here
