;;; package --- Summary -*- lexical-binding: t;-*-
;;; Commentary:
;;; Code:

;; :init executes code BEFORE a package is loaded
;; :config executes code AFTER a package is loaded

(use-package bug-hunter
	:defer t)


;(use-package exec-path-from-shell
;  :init
;  (exec-path-from-shell-initialize)
;  (exec-path-from-shell-copy-envs '("LIBRARY_PATH" "INFOPATH" "CPATH" "MANPATH")))


(use-package treesit-auto
	  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


;(use-package treesit-fold
;  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold"))

;(use-package css-in-js-mode :straight '(css-in-js-mode :type git :host github :repo "orzechowskid/tree-sitter-css-in-js"))


(use-package evil
  :init
  :config
	(setq evil-want-minibuffer nil)
  (evil-set-undo-system 'undo-redo)
  (evil-set-initial-state 'package-menu-mode 'motion)
  (evil-mode 1))


(use-package evil-collection
  :defines evil-collection-company-use-tng
  :after evil
  :config
	(evil-set-initial-state 'archive-mode 'normal)
	(evil-set-initial-state 'occur-mode 'normal)
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
 		 debugger
 	   man
 	   woman
 	   completion
 	   helpful))
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))


;; In visual-state with S<textobject> or gS<textobject
;; In normal-state with ys<textobject> or yS<textobject>
(use-package evil-surround
	:config
	(global-evil-surround-mode 1))


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
    doom-modeline-vcs-max-length 60
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


(use-package magit
	:init
	(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


(use-package git-timemachine
  :init
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
  ;:custom
  ;(dired-subtree-line-prefix "  ")
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


;; https://doc.emacsen.de/gallery.html
;(use-package doom-themes
;  :config
;  ;(set-background-color "black")
;  (load-theme 'doom-nord t))  ; doom-acario-light  doom-nord    doom-nord-light   doom-city-lights   doom-outrun-electric   doom-wilmersdorf   doom-material    doom-manegarm
;(use-package ef-themes
;  :config
;  (load-theme 'ef-winter t)) ; ef-dark  ef-duo-dark  ef-deuteranopia-light  ef-deuteranopia-dark  ef-maris-light   ef-elea-light  ef-winter   ef-night   ef-cherie
;(use-package modus-themes
;	:config
;  (load-theme 'modus-vivendi t)) ; modus-operandi  modus-vivendi
;(use-package nano-theme
;  :config
;	(set-face-attribute 'font-lock-string-face nil :foreground "Orange")
;	(set-background-color "black")
;  (load-theme 'nano-dark t)) ; nano-light  nano-dark
;(use-package iceberg-theme
;	:config
;  (iceberg-theme-create-theme-file)
;  (load-theme 'solarized-iceberg-dark t))
;(use-package nord-theme
;	:config
;	(load-theme 'nord t)
;	(set-background-color "black"))
;(use-package catppuccin-theme
;  :config
;  (catppuccin-load-flavor 'macchiato))  ; latte mocha macchiato frappe
;(use-package color-theme-sanityinc-tomorrow
;	:config
;	(load-theme 'sanityinc-tomorrow-eighties t))
;(use-package south-theme
;  :vc (:url "https://github.com/SophieBosio/south"
;       :rev :newest
;       :branch "main"))
;(use-package nordic-night-theme
;	:config
;	(load-theme 'nordic-night t))
;(use-package monokai-pro-theme
;	:ensure t
;  :config
;  (load-theme 'monokai-pro t))
;(use-package doric-themes
;	:ensure t
;  :demand t
;  :config
;	(doric-themes-select 'doric-light))
(add-to-list 'custom-theme-load-path "~/.emacs.d/private/themes/")
(load-theme 'doom-navy-copper t) ;  doom-navy-copper  doom-orange-grey  doom-purple-gold   doom-cyan-charcoal   doom-silver-slate
;(load-theme 'naga-blue t)
;(load-theme `tron t)


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
  (set-face-attribute 'tab-bar-tab nil :foreground "orange" :background 'unspecified)
  (set-face-attribute 'tab-bar-tab-inactive nil :foreground (face-foreground 'default) :background 'unspecified :box nil)
  (set-face-attribute 'tab-bar-tab-group-inactive nil :foreground 'unspecified :background 'unspecified :box nil)
  )



(use-package vertico
  ;; vertico provides mini-buffer completion
  :config
  (setq vertico-count 20)
  (setq vertico-resize nil)
  :init
  (vertico-mode)
	(vertico-multiform-mode))


(use-package vertico-posframe
	:config
	;(setq vertico-posframe-parameters '((alpha . 85)))
  :init
  (vertico-posframe-mode 1))
(setq vertico-multiform-commands
		'((consult-line (:not posframe))
		(t posframe)))


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
	:config
	;; The issue of eglot-rename not working seamlessly with embark-identifier often stems from how eglot-rename expects its input and how Embark delivers it.
  ;; Specifically, eglot-rename is designed to be called with a specific input, typically a symbol at point, and it then prompts for the new name. When called via embark-identifier,
	;; Embark might try to "insert" the identified target directly into the command, which is not the expected behavior for eglot-rename's interactive prompt.
  ;; To address this, a common solution involves configuring Embark to allow eglot-rename to prompt for input instead of trying to pre-fill it. This is achieved by adding eglot-rename
	;; to embark-target-injection-hooks, ensuring that Embark does not attempt to "inject" the identifier directly into the command.
	(push 'embark--allow-edit (alist-get 'eglot-rename embark-target-injection-hooks))
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


(use-package consult-xref-stack
  :vc
  (:url "https://github.com/brett-lempereur/consult-xref-stack" :branch "main"))


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


(use-package nerd-icons :defer t)
(use-package nerd-icons-dired
	:config
	(nerd-icons-dired-mode))
(setq dired-sidebar-theme 'nerd-icons)


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
	;                    https://elpa.gnu.org/devel/doc/eglot.html#index-eglot_002dcode_002daction_002dindications
  ; 
  ; M-x eglot-workspace-configuration
  :config
  ; https://www.reddit.com/r/emacs/comments/vau4x1/comment/ic6wd9i/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  ; Eglot writes events to an events-buffer that can become very large thus slowing emacs down
	(message "warning: `jsonrpc--log-event' is ignored.")
  ;(add-to-list 'eglot-server-programs '(elixir-ts-mode "/Users/robertcarter/Developer/elixir/elixir-ls-v0.24.1/language_server.sh"))
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "/opt/homebrew/bin/elixir-ls"))
	(add-to-list 'eglot-server-programs '((python-mode python-ts-mode) "basedpyright-langserver" "--stdio"))
	(add-to-list 'eglot-server-programs '(go-mode "gopls"))
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp"))
  (add-to-list 'eglot-server-programs '((sql-mode) "sql-language-server" "up --method stdio"))
	(add-to-list 'eglot-server-programs '((html-mode html-ts-mode) "vscode-html-language-server" "--stdio"))
	(add-to-list 'eglot-server-programs '(json-ts-mode "vscode-json-language-server" "--stdio"))
	(add-to-list 'eglot-server-programs '(css-ts-mode "vscode-css-language-server"  "--stdio"))

	(add-to-list 'project-vc-extra-root-markers "tsconfig.json")
	; For python ?
	; https://www.reddit.com/r/emacs/comments/1mddqpx/eglot_pyright_made_simple_thanks_to_uvx/
	; (add-to-list 'eglot-server-programs '((html-ts-mode :language-id "html") . ("tailwindcss-language-server")))
  (add-to-list 'eglot-server-programs '((typescript-mode tsx-ts-mode -js-mode) "typescript-language-server" "--stdio"))
	;; taken from https://www.reddit.com/r/emacs/comments/11faie2/how_can_i_make_eglot_shut_up_in_the_minibuffer/
	;; add the following to fix eldoc overriding the mini-buffer with function signature docs instead of
	;; showing the current flycheck error information.
	(defun /eglot-managed-mode-initialize ()
		(setq-local
				eldoc-documentation-functions
				(list
					;; #'eglot-signature-eldoc-function
					;; #'eglot-hover-eldoc-function
					;; #'flymake-eldoc-function
				)))
	;; (add-hook 'eglot-managed-mode-hook #'/eglot-managed-mode-initialize)

  ;; setting language specific lsp configs
  ;;(setq-default eglot-workspace-configuration
  ;;  '((elixir-ls
	;;(elixirLS.autoInsertRequiredAlias . nil))))
	;(setq eglot-quickfix-function #'ignore)

	;; stops eglot from showing available code actions in the echo area
  (setq eglot-code-action-indications nil)
  (setq eglot-events-buffer-size 0)
  (setq eglot-autoshutdown t)

  :defer t
  :hook (
		(html-ts-mode . eglot-ensure)
	  (ruby-ts-mode . eglot-ensure)
	  (python-ts-mode . eglot-ensure)
	  (elixir-ts-mode . eglot-ensure)
	  (go-mode . eglot-ensure)
	  (js-mode . eglot-ensure)
		(js-ts-mode . eglot-ensure)
		(tsx-ts-mode . eglot-ensure)
	  (typescript-ts-mode . eglot-ensure)
	  (sql-mode . eglot-ensure)))

;; ignore jsonrpc events to speed up eglot
(fset #'jsonrpc--log-event #'ignore)

(with-eval-after-load 'eglot
  (setq completion-category-defaults nil))

;; activate documentation popup on hover
;(with-eval-after-load 'eglot
	;(eldoc-box-hover-at-point-mode))

;(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode)

;; popup ui for eldoc documentation
;(use-package eldoc-box)


(use-package eglot-signature-eldoc-talkative)


(use-package eshell
  :config
  (require 'em-smart)
  ;(setq eshell-prompt-function
  ;   (lambda ()
  ;     (concat
  ;      (propertize "\n┌─ " 'face `(:foreground "royal blue"))
  ;      (propertize (concat (eshell/pwd)) 'face `(:foreground "SteelBlue1"))
  ;      (propertize " (" 'face `(:foreground "green"))
  ;      (if (magit-get-current-branch)
  ;          (propertize (magit-get-current-branch) 'face `(:foreground "green"))
  ;          (propertize "z" 'face `(:foreground "yellow")))
  ;      (propertize ")" 'face `(:foreground "green"))
  ;      (propertize "\n" 'face `(:foreground "green"))
  ;      (propertize "└─>" 'face `(:foreground "royal blue"))
  ;      (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "DeepSkyBlue1"))  ;; prompt color
  ;      )))

	;; https://tony-zorman.com/posts/eshell-zsh-history.html
	(defun slot/unmetafy ()
  (cl-flet ((unmetafy (input)
              (let ((i 0) output)
                (while-let ((char (nth i input))
                            (inc-and-char
                             (if (= char #x83)
                                 ;; Skip meta character and unmetafy.
                                 `(2 . ,(logxor (nth (1+ i) input) 32))
                               ;; Advance as usual.
                               `(1 . ,char))))
                  (cl-incf i (car inc-and-char))
                  (setq output (cons (cdr inc-and-char) output)))
                (decode-coding-string
                 (apply #'unibyte-string (nreverse output))
                 'utf-8-unix
                 t))))
    (let ((hist-file "~/.zsh_history"))
      (with-temp-buffer
        (insert (mapconcat (-compose #'unmetafy #'string-to-list)
                           (s-lines (f-read-bytes hist-file))
                           "\n"))
        (write-file hist-file)))))

	(defun slot/eshell-exit (&optional arg)
		"Exit eshell and kill the current frame."
		(interactive "P")
		(slot/unmetafy)
		(eshell-write-history)
		(save-buffers-kill-terminal))

	(setq eshell-history-file-name "~/.zsh_history")
	;(setq eshell-history-file-name (expand-file-name "eshell/history" user-emacs-directory))
  (setq eshell-banner-message ""
    eshell-smart-space-goes-to-end t
		eshell-save-history-on-exit t
    eshell-history-size 1000
    eshell-highlight-prompt t
		eshell-scroll-show-maximum-output nil
    ;eshell-scroll-to-bottom-on-input t
    eshell-hist-ignoredups t)
	:hook (
					(eshell-hist-load . slot/unmetafy)
					(eshell-exit-hook . slot/eshell-exit)))

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-git-root t)
  (eshell-toggle-run-command nil)
  ;(eshell-toggle-init-function #'eshell-toggle-init-ansi-term)
  :bind
  ("s-`" . eshell-toggle))


(use-package eshell-git-prompt
	:init
	(eshell-git-prompt-use-theme 'powerline))


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


; https://console.anthropic.com/settings/billing
; TODO configure to use Anthropic instead, reference API key via environment variable stored in zshrc
; NOTE: you can invoke gptel's transient menu with either \ + C-u + gptel-send   or with gptel-menu
(use-package gptel
  :ensure t
	:config
	(setq gptel-model 'gpt-4o
		gptel-default-mode 'org-mode
		gptel-backend (gptel-make-gh-copilot "Copilot")))


(use-package gptel-quick
	; NOTE: Press '+' for an expanded summary
  :vc (:url "https://github.com/karthink/gptel-quick" :rev :newest)
	:init
	(setq gptel-quick-timeout 1000))


; NOTE:  API key and models saved to /Users/robertcarter/.config/eca/config.json
; BILLING:  https://console.anthropic.com/settings/billing
(use-package eca
  :bind (("C-c a" . eca-run)
         ("C-c C-a" . eca-run)
         ("C-c e" . eca-explain)
         ("C-c r" . eca-refactor)
         ("C-c f" . eca-fix)
         ("C-c d" . eca-document)
         ("C-c t" . eca-test))
  :config
  ;; Optional: Set ECA preferences
  (setq eca-auto-save-before-run t)
  (setq eca-show-output-buffer t))


(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
  :init
  (global-undo-tree-mode))


(use-package avy)


(use-package org
	:init
	(add-hook 'org-mode-hook #'visual-line-mode)
  :config
	(setq org-todo-keywords
		'((sequence "TODO" "DOING" "|" "DONE" "CANCELLED")))
	;; Log time a task was set to DONE.
	(setq org-log-done (quote time))

	;; Don't log the time a task was rescheduled or redeadlined.
	(setq org-log-redeadline nil)
	(setq org-log-reschedule nil)
	(setq org-agenda-files '("~/Notes/org-roam-notes/20210905175557-todo.org.gpg" "~/Notes/org-roam-daily"))
	(set-face-attribute 'org-agenda-date-today nil
		:foreground "orange"
		:background 'unspecified
	)
	(setq org-agenda-window-setup 'current-window)
	(setq org-archive-location "~/Notes/org-roam-notes/archived.org::")

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
  (setq org-startup-indented t)
  (setq org-src-preserve-indentation t)
	;(setq org-src-fontify-natively t
	;  org-src-tab-acts-natively t
  ;    org-edit-src-content-indentation 0) 
  (setq org-adapt-indentation t)
	;(setq org-clock-sound "~/.emacs.d/private/nyan.wav")

	;; Resize Org headings
	(dolist (face '((org-document-title . 4.0)
									 (org-level-1 . 1.35)
									 (org-level-2 . 1.3)
									 (org-level-3 . 1.2)
									 (org-level-4 . 1.1)
									 (org-level-5 . 1.1)
									 (org-level-6 . 1.1)
									 (org-level-7 . 1.1)
									 (org-level-8 . 1.1)))
		(set-face-attribute (car face) nil :font "Source Sans Pro" :weight 'bold :height (cdr face)))

	;; Make the document title a bit bigger
	(set-face-attribute 'org-document-title nil :font "Source Sans Pro" :weight
		'bold :height 1.8)

	(require 'org-indent)
	(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
	(add-hook 'org-mode-hook 'variable-pitch-mode)

	(set-face-attribute 'org-block nil            :foreground nil :inherit
		'fixed-pitch :height 0.85)
	(set-face-attribute 'org-code nil             :inherit '(shadow fixed-pitch) :height 0.85)
	(set-face-attribute 'org-indent nil           :inherit '(org-hide fixed-pitch) :height 0.85)
	(set-face-attribute 'org-verbatim nil         :inherit '(shadow fixed-pitch) :height 0.85)
	(set-face-attribute 'org-special-keyword nil  :inherit '(font-lock-comment-face
	fixed-pitch))
	(set-face-attribute 'org-meta-line nil        :inherit '(font-lock-comment-face fixed-pitch))
	(set-face-attribute 'org-checkbox nil         :inherit 'fixed-pitch)

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
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

  :bind (
	  :map org-mode-map
					("TAB" . org-cycle)
					("C-j" . windmove-down)
          ("C-k" . windmove-up)
          ("C-p" . org-roam-node-find)
          ("C-f" . consult-ripgrep)
          ("C-c n" . org-roam-capture)
          ("C-'" . org-roam-buffer-toggle)
          ("C-c i" . org-roam-node-insert))
	)


(use-package org-recur
  :hook ((org-mode . org-recur-mode)
         (org-agenda-mode . org-recur-agenda-mode))
  :demand t
  :config
  (define-key org-recur-mode-map (kbd "C-c x") 'org-recur-finish)

  ;; Rebind the 'x' key in org-agenda (default: `org-agenda-day-view').
  (define-key org-recur-agenda-mode-map (kbd "x") 'org-recur-finish)
  (define-key org-recur-agenda-mode-map (kbd "C-c x") 'org-recur-finish)

  (setq org-recur-finish-done t
        org-recur-finish-archive t))


(use-package org-alert
  :ensure t
	:config
	(setq org-alert-interval 300
		org-alert-notify-cutoff 10
    org-alert-notify-after-event-cutoff 10
		org-alert-notification-title "Task Due"
		alert-default-style 'osx-notifier)
	(org-alert-enable))


(use-package org-superstar
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-special-todo-items t) ;; Makes TODO header bullets into boxes
  (setq org-superstar-todo-bullet-alist '(("TODO" . 9744)
																					 ("DONE" . 9744)
																					 ("READ" . 9744)
																					 ("IDEA" . 9744)
                                           ("WAITING" . 9744)
                                           ("CANCELLED" . 9744)
                                           ("PROJECT" . 9744)
                                           ("POSTPONED" . 9744)))
	)


(use-package org-roam
	:defer t
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
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg"
							 "# -*- mode:org; epa-file-encrypt-to: (\"robertcliffordcarter@gmail.com\") -*- 
#+title: ${title}
#+startup: showall inlineimages
#+tags: %^{org-roam-tags}
#+created: %u
#+options: ^:{}

")
	    :unnarrowed t))))


(use-package org-roam-ui
	:defer t
  :config
  (setq org-roam-ui-follow t))


(use-package org-drill
	:defer t
  :defines org-drill-hint-separator org-drill-left-close-delimiter org-drill-right-close-delimiter
  :init
  (setq org-drill-scope 'file)
  (setq org-drill-add-random-noise-to-intervals-p t)
  (setq org-drill-hint-separator "||")
  (setq org-drill-left-close-delimiter "<[")
  (setq org-drill-right-close-delimiter "]>")
  (setq org-drill-learn-fraction 0.25))


(use-package org-download
	:defer t
  :after org
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")
  (org-image-actual-width 300)
  (org-download-screenshot-method "/opt/homebrew/bin/pngpaste %s")
	:hook
	(org-mode . org-download-enable)
  :config
  (require 'org-download)
  (add-hook 'org-mode-hook 'org-download-enable))


(use-package simple-httpd)


(use-package websocket)


(use-package restclient
	:init
	(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
	:config
	(defun restclient-replace-request-with-response (&rest _args)
  "Replace the current buffer (request) content with the HTTP response."
  (let ((response-buffer "*HTTP Response*"))
    (when (get-buffer response-buffer)
      (let ((response (with-current-buffer response-buffer (buffer-string))))
        (erase-buffer)
        (insert response)
        (kill-buffer response-buffer)))))
	(advice-add 'restclient-http-handle-response :after #'restclient-replace-request-with-response))


(use-package restclient-jq
	:after restclient)


(use-package nvm)


(use-package hydra)

    
(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
    '(("python" (black))
       ("gO"    (GOFMT) (GOIMPORTS))
       ("js"    (PRETTIER))
			 ("ruby"  (rubocop "--auto-correct"))
       ("ts"    (PRETTIER))
       ("html"  (PRETTIER))
       ("css"   (PRETTIER))
       ("json"  (PRETTIER))
       ("yaml"  (PRETTIER))
       ("xml"   (PRETTIER))
       ("sql"   (SQLFORMAT)))))


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


(use-package jtsx
  :ensure t
  :mode (("\\.jsx?\\'" . jtsx-tsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode)))


(use-package disaster
  :init
  ;; If you prefer viewing assembly code in `nasm-mode` instead of `asm-mode`
  (setq disaster-assembly-mode 'asm-mode))


(use-package corfu
  ;; corfu provides in-buffer completion with a small completion popup
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.3)
  (corfu-min-width 30)
  (corfu-popupinfo-min-width 60)
  (corfu-quit-no-match 'separator)
  (setopt corfu-on-exact-match 'show)
  :bind
  (:map corfu-map
    ("M-n" . corfu-next)
    ("M-p" . corfu-previous))
  :config
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
	(add-hook 'eshell-mode-hook (lambda ()
                              (setq-local corfu-auto nil)
                              (corfu-mode)))
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


(use-package cape
  :after corfu
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
	:init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-history)
	)


(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs '( "~/.emacs.d/snippets"))
  (setq yas-indent-line 'auto)
  (yas-global-mode 1))


(use-package yasnippet-snippets
  :after yasnippet
  :init
  (add-to-list 'yas-snippet-dirs yasnippet-snippets-dir))


(use-package yasnippet-capf)


; https://stackoverflow.com/questions/72601990/how-to-show-suggestions-for-yasnippets-when-using-eglot?rq=3
(defun my/eglot-capf ()
  (setq-local completion-at-point-functions
    (list (cape-capf-super
	    #'eglot-completion-at-point
	    ;#'yasnippet-capf
	    #'cape-history
	    #'cape-file))))
(add-hook 'eglot-managed-mode-hook #'my/eglot-capf)


(use-package inf-ruby)


(use-package rspec-mode)
(eval-after-load 'rspec-mode
 '(rspec-install-snippets))


(use-package nodejs-repl)


(use-package inf-elixir
	:defer t
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
  (setq flycheck-highlighting-mode 'lines)
  (setq flycheck-indication-mode 'left-fringe)
	(setq flycheck-python-pyright-executable "/opt/homebrew/bin/ruff")
  (setq flycheck-ruby-executable "/Users/robertcarter/.rbenv/shims/rubocop")
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
  :custom (flycheck-eglot-exclusive nil)
  :config
  (global-flycheck-eglot-mode 1))


(use-package prodigy
	:defer t
  :config
  ;(prodigy-define-service
  ;  :name "Elixir Server"
  ;  :command "mix"
  ;  :args '("phx.server")
  ;  :cwd "/Users/robertcarter/Developer/elixir/elixir-phoenix"
  ;  :tags '(elixir)
  ;  :stop-signal 'kill
  ;  :kill-process-buffer-on-stop t)
	(prodigy-define-service
		:name "Client"
		:command "npm"
		:args '("run" "dev")
		:cwd "/Users/robertcarter/Developer/typescript/portfolio"
		:tags '(react)
		:stop-signal 'kill
		:kill-process-buffer-on-stop t)
  (prodigy-define-service
    :name "Server"
    ;:command "bundle"
    ;:args '("exec" "rails" "server")
		:command "rails"
		:args '("s")
    :cwd "/Users/robertcarter/Developer/rails/portfolio_api"
		;:command "unicorn"
		;:args '("-c" "config/unicorn.rb")
    ;:cwd "/Users/robertcarter/Developer/rails/portfolio_api"
    :tags '(rails)
    :stop-signal 'kill
    :kill-process-buffer-on-stop t))


(use-package outline-indent
  :ensure t
  :custom
  (outline-indent-ellipsis " ▼ "))
(add-hook 'prog-mode-hook #'outline-indent-minor-mode)


(use-package better-jumper
	:init
	(better-jumper-mode +1))


(use-package go-mode)


(use-package go-playground)

(use-package bookmark+
	:load-path "~/.emacs.d/private/bookmark-plus")


; /rpc:user@host:/path/to/file
;(use-package tramp-rpc
;  :ensure t)

(use-package which-key
	:config
	(setq which-key-popup-type 'frame)
	(which-key-mode))

;; NOTE: popper is loaded last because otherwise it does not
;; seem to be initialized correctly and i have to manually stop/start
;; it again when emacs starts up. which is annoying
;; https://github.com/gilbertw1/better-jumper/issues/21
(use-package popper
	:ensure t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
	:init
	(setq popper-reference-buffers
		'("\\*prodigy\\*"    prodigy-mode
			 "\\*Python\\*"     inferior-python-mode
			 "\\*nodejs\\*"     nodejs-repl-mode
			 "\\*ruby\\*"       inf-ruby-mode
			 "\\*SQL: SQLite\\*"
			 "\\*Messages\\*"
			 "\\*Warnings\\*"
			 "\\*Help\\*"
			 "\\*Copilot\\*"
			 "Output\\*$"
			 "\\*Async Shell Command\\*"
			 "\\*HTTP Response\\*"
			 help-mode
			 compilation-mode
			 ))
	(setq popper-window-height 0.50)
  (popper-mode +1))

;; Always reuse existing compilation window.
(push '("\\*compilation\\*" . (nil (reusable-frames . t))) display-buffer-alist)

;; configure xref to reuse current buffer instead of hijacking another
(add-to-list 'display-buffer-alist
             '("\\*xref\\*"
               (display-buffer-same-window)))
;;; packages.el ends here
