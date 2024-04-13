;;; package --- Summary
;;; Commentary:
;;; Code:

;; :init executes code BEFORE a package is loaded
;; :config executes code AFTER a package is loaded

(use-package bug-hunter)


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
    doom-modeline-height 30
    doom-modeline-buffer-encoding nil
    doom-modeline-display-misc-in-all-mode-lines nil
    doom-modeline-percent-position nil
    doom-modeline-env-enable-ruby nil
    doom-modeline-env-version nil)
  :hook (after-init . doom-modeline-mode))


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
  :custom
  (dired-subtree-line-prefix "  ")
  :bind
  (:map dired-sidebar-mode-map ("<return>" . 'dired-sidebar-find-file-alt)))


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


(use-package ef-themes
  ;:config
  ;(load-theme 'ef-maris-light t) ; ef-duo-dark  ef-deuteranopia-light  ef-deuteranopia-dark  ef-maris-light   ef-elea-light  ef-winter   ef-night
)
(use-package modus-themes
  :config
  (load-theme 'modus-vivendi t)) ; modus-operandi  modus-vivendi
(use-package nano-theme) ; nano-light  nano-dark


(use-package tabspaces
  :hook
  (after-init . tabspaces-mode)
  (after-init . tabspaces-reset-buffer-list)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-remove-to-default t))


(use-package vertico
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
  (setq display-buffer-alist '(("\\*Embark Export: .*" (display-buffer-reuse-mode-window display-buffer-below-selected))))
  (setq embark-indicator #'embark-mixed-indicator)
  (setq embark-verbose-indicator-display-action
        '(display-buffer-at-bottom
  	  (window-height . fit-window-to-buffer)))
  :bind
  (("M-o" . embark-act)))


(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package consult
  :defines consult-project-root-function
  :after vertico
  :config
  (setq consult-project-root-function (lambda () (project-root (project-current)))))


(use-package marginalia
  :init
  (marginalia-mode))


(use-package all-the-icons)


(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))


(use-package eglot
  :config
  ; https://www.reddit.com/r/emacs/comments/vau4x1/comment/ic6wd9i/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  ; Eglot writes events to an events-buffer that can become very large thus slowing emacs down
  (setq eglot-events-buffer-size 0)
  :defer t
  :hook (
     (ruby-mode . eglot-ensure)
	 (python-ts-mode . eglot-ensure)
	 (go-mode . eglot-ensure)
	 (js-mode . eglot-ensure)
	 (typescript-ts-mode . eglot-ensure)
	 (sql-mode . eglot-ensure)))


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
        (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "royal blue"))
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
  :vc (copilot :url "https://github.com/copilot-emacs/copilot.el"
	:branch "main")
  :config
  (define-key copilot-mode-map (kbd "M-n") #'copilot-next-completion)
  (define-key copilot-mode-map (kbd "M-p") #'copilot-previous-completion)
  :hook
  (prog-mode . copilot-mode))


(use-package company
  :config
  (setq company-idle-delay 50.0)
  (global-company-mode 1))


(use-package company-box
  :hook (company-mode . company-box-mode))


(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
  :init
  (global-undo-tree-mode))


(use-package better-jumper
  :after evil
  :init
  (better-jumper-mode 1))


(use-package avy)


(use-package org
  :config
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


(use-package org-drill
  :defines org-drill-hint-separator org-drill-left-close-delimiter org-drill-right-close-delimiter
  :init
  (setq org-drill-scope 'file)
  (setq org-drill-add-random-noise-to-intervals-p t)
  (setq org-drill-hint-separator "||")
  (setq org-drill-left-close-delimiter "<[")
  (setq org-drill-right-close-delimiter "]>")
  (setq org-drill-learn-fraction 0.25))


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


(use-package flycheck
  :init
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-default flycheck-indication-mode 'left-fringe)
  (setq flycheck-python-pylint-executable "/opt/homebrew/bin/pylint")
  :config
  (global-flycheck-mode +1))

  ;(add-hook 'after-init-hook #'global-flycheck-mode))
    
    
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


(use-package inf-ruby)


(use-package nodejs-repl)

(provide 'packages)
;;; packages.el ends here
