;;; package --- Summary
;;; Commentary:
;;;
;;; https://github.com/cmacrae/.emacs.d#perspective    (also very good)

;;; https://emacs.nasy.moe/#orge7b5d89    (this one is good)
;;; https://ladicle.com/post/config/#lsp
;;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
;;; https://menno.io/posts/use-package/
;;; https://github.com/bmag/imenu-list
;;; https://blog.sumtypeofway.com/posts/emacs-config.html  (lists some interesting packages for investigation)
;;; https://github.com/dajva/rg.el (ripgrep integration)
;;; https://luca.cambiaghi.me/vanilla-emacs/readme.html#h:6E4E5BD6-1930-4DCE-8E26-5ADAC2B9A152   (has some packages listed worth investigating)
;;; https://www.reddit.com/r/emacs/comments/gr72by/how_do_you_guys_refine_search_results_doom_emacs/ (small discussion on how to use ripgrep for searching)
;;; https://www.youtube.com/watch?v=AaUlOH4GTCs (simple tutorial on how to add custom ivy action options... when you press alt-o to show additional options for an ivy list)
;;; https://www.reddit.com/r/emacs/comments/kqutap/selectrum_prescient_consult_embark_getting_started/gi6yibq/     INVESTIGATE THESE
;;; https://raw.githubusercontent.com/txgvnn/dots/master/.emacs  (example config)
;;;
;;; TODO
;;;  - add keybinding to move to next/previous diagnostic error
;;;  - https://org-roam.discourse.group/t/what-does-it-feel-like-to-work-with-10-000-notes-in-org-roam-benchmarking-org-roams-search-methods/227
;;;  - read thoroughly: https://emacsair.me/2017/09/01/magit-walk-through/
;;;
;;; NOTE
;;;
;;;   In order to present additional actions when acting on a selection, for instance "find-file" (ctrl-x ctrl-f) you have to invoke the function 'counsel-find-file'.
;;;   Then you trigger the UI for the additional actions with ctrl-o
;;;
;;; With counsel-rg you can pass any flags to ripgrep after --
;;;
;;; <ctrl-f> "def"                    -> look in all files
;;; <ctrl-f> "def -- g*.el            -> look in all *.el files
;;; <ctrl-f> "def -- g*.el!ivy.el     -> look in all *.el files except ivy.el
;;;
;;; Code:

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
 '(help-at-pt-display-when-idle '(flymake-diagnostic) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.1)
 '(package-selected-packages
	 '(org-roam company yasnippet embark-consult embark marginalia consult rainbow-delimiters orderless dashboard vterm treemacs-all-the-icons treemacs persp-projectile perspective company-box org lsp-ui go-mode bug-hunter use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "ETBembo" :height 6.0 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "ETBembo" :height 2.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "ETBembo" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "ETBembo" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "ETBembo" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "ETBembo"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "ETBembo"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "ETBembo"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "ETBembo")))))

(defvar evil-want-C-u-scroll)
(defvar ac-cons-threshold)
(defvar all-the-icons-dired-monochrome)

(setq evil-want-C-u-scroll t)
(setq initial-scratch-message "")
(setq gc-cons-threshold 100000000) ;; increase garbage-collection threshold to 100mb from 8kb
(setq all-the-icons-dired-monochrome nil)
(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)
(fset 'yes-or-no-p 'y-or-n-p)  ;; use 'y' and 'n' for 'yes' and 'no'

(setq completion-ignore-case t)

;; PACKAGES ==============================================================================================================================================================================================================================================

(use-package bug-hunter)

(use-package evil
  :init
	(setq evil-want-keybinding nil)
	:config
	(define-key evil-motion-state-map (kbd "C-z") nil)
	(define-key evil-motion-state-map (kbd "RET") nil)
	(define-key evil-motion-state-map (kbd "C-f") nil)
	(define-key evil-normal-state-map (kbd "-") 'dired)
	(define-key evil-normal-state-map (kbd "C-s") 'projectile-persp-switch-project)
	(define-key evil-normal-state-map (kbd "C-p") 'project-find-file)
	(define-key evil-normal-state-map (kbd "C-b") 'switch-to-buffer)
	(define-key evil-normal-state-map (kbd "C-n") 'treemacs)
	(define-key evil-normal-state-map (kbd "z-c") 'hs-hide-block)
	(define-key evil-normal-state-map (kbd "z-o") 'hs-show-block)
  (evil-mode))

(use-package evil-collection
	:defines evil-collection-company-use-tng
	:after evil
	:config
	(setq evil-collection-company-use-tng nil)
	(evil-collection-init))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package selectrum
	:config
	(setq selectrum-max-window-height (/ (frame-height) 2))
	(setq selectrum-fix-vertical-window-height t)
	(selectrum-mode +1))

(use-package consult)

(use-package popwin
  :init
  (popwin-mode 1))

(use-package lsp-mode
	:commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
	(setq lsp-headerline-breadcrumb-enable nil)
  :hook (ruby-mode . lsp-deferred)
	:bind (:map lsp-mode-map
							("TAB" . completion-at-point)))

(use-package projectile
  :init
  (projectile-mode +1))

(use-package perspective
	:init
	:config (persp-mode))

(use-package persp-projectile
	:after (perspective))

(use-package treemacs
	:defer t
	:config
	(progn
		(setq treemacs-show-hidden-files t
					treemacs-display-in-side-window t
					treemacs-position 'left
					treemacs-width 35)))

(use-package treemacs-all-the-icons
	:after (treemacs)
	:config
	(treemacs-load-theme 'all-the-icons))

(use-package treemacs-perspective
	:after (treemacs perspective)
	:config
	(treemacs-set-scope-type 'Perspectives))

(use-package lsp-ui
  :after (lsp-mode)
  :config
	(setq lsp-ui-doc-position 'at-point)
	(setq lsp-enable-snippet nil))

(use-package magit)

(use-package doom-modeline
	:init
	(setq doom-modeline-workspace-name nil)
	(setq doom-modeline-buffer-encoding nil)
	(setq doom-modeline-vcs-max-length 24)
	:hook (after-init . doom-modeline-mode))

(use-package evil-leader
	:after evil
	:functions evil-leader/set-leader
	:config
	(global-evil-leader-mode)
	(add-to-list 'evil-buffer-regexps '("*Packages*" . normal)) ;; enable evil in packages-menu
	(evil-leader/set-leader ",")
	(evil-leader/set-key
	 "cp" 'copy-filepath-to-clipboard
	 "q" 'delete-window
	 "o" 'delete-other-windows
	 "r" 'lsp-find-references
	 "f" 'lsp-find-definition
	 "d" 'flycheck-list-errors
	 "gl" 'magit-log-all
	 "gb" 'magit-blame
	 "gs" 'magit-status
	 "gc" 'magit-branch
	 "gh" 'magit-log-buffer-file)
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

(use-package flycheck
	:init (global-flycheck-mode))

(use-package doom-themes
	:defines doom-themes-enable-bolt
 	:config
 	(setq doom-themes-enable-bolt t
 				doom-themes-enable-italic t)
 	(load-theme 'doom-city-lights t)) ;; doom-nord  doom-wilmersdorf  doom-city-lights  doom-sourcerer

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

(use-package company
	:config (progn
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
	(setq org-return-follows-link t)
	(setq org-pretty-entities t)
	(setq org-hide-emphasis-markers t)
	:hook (prog-mode . yas-minor-mode))

(use-package org-roam
	:hook
	(after-init . org-roam-mode)
	:custom
	(org-roam-directory (file-truename "~/notes/org-roam-notes/"))
	:init
	(setq org-roam-db-update-method 'immediate)
	(setq org-roam-dailies-directory "~/notes/org-roam-daily")
	(setq org-roam-capture-templates
				'(("d" "default" plain #'org-roam-capture--get-point
					 :file-name "%<%Y-%m-%d>-${slug}"
					 :head "#+title: ${title}\n#+ROAM_TAGS: %^{org-roam-tags}\n#+created: %u\n#+last_modified: %U\n%?"
					 :unnarrowed t
					 :jump-to-captured t)
					("l" "clipboard" plain #'org-roam-capture--get-point "%i%a"
					 :file-name "%<%Y%m%d%H%M%S>-${slug}"
					 :head "#+title: ${title}\n#+created: %u\n#+last_modified: %U\n#+ROAM_TAGS: %? \n"
					 :unnarrowed t
					 :prepend t
					 :jump-to-captured t)))
	:bind (
				 :map org-mode-map
							("C-x C-f" . org-roam-find-file)
							("C-f" . consult-ripgrep)
							("C-'" . org-roam)
              ("C-c i" . org-roam-insert)
              ("C-c I" . org-roam-insert-immediate)))

(use-package orderless
  :ensure t
  :custom (completion-styles '(orderless)))

(use-package vterm)

(use-package dashboard
	:config
	(setq dashboard-items '((projects . 5)
													(agenda . 5)))
	(dashboard-setup-startup-hook))

(use-package rainbow-delimiters)

(use-package marginalia
	:init
	(marginalia-mode))

(use-package embark
	:after selectrum
	:bind
	(("M-o" . embark-act))
	:init
	(setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package erc
	:config
	(defun rgr/erc-switch-to-channel(&optional channel)
		(when (string= (or channel "#emacs") (buffer-name (current-buffer)))
    (switch-to-buffer (current-buffer))))
	(defun rgr/erc-start()
		(interactive)
		(unless(get-buffer "irc.libera.chat:6697")
    (progn
      (erc-tls :server "irc.libera.chat" :port "6697")
      (add-hook 'erc-join-hook 'rgr/erc-switch-to-channel))))
  (setq erc-fill-function 'erc-fill-static)
  (setq erc-fill-static-center 22)
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-lurker-threshold-time 43200)
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-server-reconnect-attempts 5)
  (setq erc-server-reconnect-timeout 3)
  (setq erc-track-exclude-types '("JOIN" "MODE" "NICK" "PART" "QUIT" "324" "329" "332" "333" "353" "477"))
	:bind ("C-c i" . #'rgr/erc-start))

;; HELP FUCNTIONS ========================================================================================================================================================================================================================================

(defun unique-shell ()
	"Create a new named shell buffer."
  (interactive)
  (call-interactively 'vterm)
  (rename-buffer (read-string "Enter buffer name: ")))

(global-set-key (kbd "C-z") #'unique-shell)

(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))

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

;; KEYBINDINGS ===========================================================================================================================================================================================================================================

(with-eval-after-load 'prog-mode (bind-key "C-'" #'lsp-ui-imenu))
(with-eval-after-load 'prog-mode (bind-key "C-f" #'consult-ripgrep))

;; C-i and C-o shouldn't jump between files in different workspaces
;; Taken from https://github.com/hlissner/doom-emacs/issues/2826
(with-eval-after-load 'persp-mode
  (defadvice persp-delete-other-windows (before better-jumper activate)
    (select-window (split-window))))

(define-key package-menu-mode-map (kbd "C-h") 'evil-window-left)
(define-key package-menu-mode-map (kbd "C-j") 'evil-window-down)
(define-key package-menu-mode-map (kbd "C-k") 'evil-window-up)
(define-key package-menu-mode-map (kbd "C-l") 'evil-window-right)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;; MODES =================================================================================================================================================================================================================================================

(show-paren-mode 1)

;; =======================================================================================================================================================================================================================================================

(tool-bar-mode -1) ;; Disable Toolbar

(add-hook 'emacs-startup-hook 'toggle-frame-maximized) ;; Make fullscreen

(setq inhibit-splash-screen t) ;; Disable splash screen

(setq initial-major-mode (quote fundamental-mode)) ;; Disable scratch screen

(setq ring-bell-function 'ignore) ;; silence alert chimes

(global-font-lock-mode 1) ;; Enable syntax highlighting

(set-default 'truncate-lines t) ;; Don't wrap lines

(toggle-scroll-bar -1) ;; Don't show scroll bars

(global-linum-mode t) ;; Show number lines

(setq scroll-step            1
      scroll-conservatively  10000) ;; Smoother scrolling with smaller steps

(defadvice split-window (after move-point-to-new-window activate)
  "Move the point to the newly created window after splitting."
  (other-window 1)) ;; Focus cursor in new split buffers

(setq-default tab-width 2) ;; Set tab space to 2

(setq org-hidden-keywords '(title))

(set-frame-font "Monaco-12" nil t)

(setq-default explicit-shell-file-name "/bin/zsh")

(setq lsp-enable-links nil)

(setq show-paren-style 'parenthesis)

(cond
  ((string-equal system-type "darwin")
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))))
(setq frame-title-format '(""))

(set-cursor-color "#00FFFF")
;; =======================================================================================================================================================================================================================================================

;;; init ends here
