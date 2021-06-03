;;; package --- Summary
;;; Commentary:
;;; 
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
;;; 
;;; TODO
;;;  - add keybinding to move to next/previous diagnostic error
;;;  - look into this:  https://github.com/ericdanan/counsel-projectile    (use ivy for projectile commands... this should allow the use of ctrl-o within an ivy-list and show additional actions
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

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(help-at-pt-display-when-idle '(flymake-diagnostic) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.1)
 '(package-selected-packages
	 '(company-box yasnippet org embark counsel simple-modeline vimish-fold imenu-list lsp-ui typescript-mode lsp-mode go-mode bug-hunter use-package all-the-icons-dired evil)))
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

(setq gc-cons-threshold 100000000) ;; increase garbage-collection threshold to 100mb from 8kb
(setq evil-want-C-u-scroll t)
(setq initial-scratch-message "")
(setq all-the-icons-dired-monochrome nil)
(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

;; PACKAGES ==============================================================================================================================================================================================================================================

(use-package counsel
	:ensure t)

(use-package evil
  :init
	(setq evil-want-keybinding nil)
	:config
	(define-key evil-normal-state-map (kbd "-") 'dired)
	(define-key evil-motion-state-map (kbd "C-z") nil)
	(define-key evil-normal-state-map (kbd "C-s") 'projectile-switch-project)
	(define-key evil-normal-state-map (kbd "C-n") 'neotree-toggle)
	(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
	(define-key evil-normal-state-map (kbd "C-b") 'ivy-switch-buffer)
	(define-key evil-normal-state-map (kbd "C-t") 'centaur-tabs-counsel-switch-group)
	(define-key evil-normal-state-map (kbd "C-f") 'counsel-rg)
	(define-key evil-normal-state-map (kbd "z-c") 'hs-hide-block)
	(define-key evil-normal-state-map (kbd "z-o") 'hs-show-block)
  (evil-mode))

(use-package evil-collection
	:after evil
	:ensure t
	:config
	(setq evil-collection-company-use-tng nil)
	(evil-collection-init))

(use-package all-the-icons)

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height-alist
      '((t
         lambda (_caller)
         (/ (frame-height) 2))))
  (ivy-mode 1)
	:config
	(define-key ivy-mode-map (kbd "C-n") 'ivy-next-line)
	(define-key ivy-mode-map (kbd "C-p") 'ivy-previous-line))

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

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1))

(use-package lsp-ui
  :after (lsp-mode)
  :config
	(setq lsp-enable-snippet nil))

(use-package magit
	:ensure t)

(use-package neotree
	:ensure t
	:config
	(setq-default neo-show-hidden-files t)
	(setq neo-theme 'icons)
	(setq neo-smart-open t)
	(setq neo-autorefresh t)
	(setq projectile-switch-project-action 'neotree-projectile-action)
  :bind ("C-c n" . neotree-toggle))

(use-package evil-leader
	:ensure t
	:after evil
	:config
	(global-evil-leader-mode)
	(add-to-list 'evil-buffer-regexps '("*Packages*" . normal)) ;; enable evil in packages-menu
	(evil-leader/set-leader",")
	(evil-leader/set-key
	 "cp" 'copy-filepath-to-clipboard
	 "q" 'delete-window
	 "n" 'neotree-find
	 "o" 'delete-other-windows
	 "r" 'lsp-find-references
	 "f" 'lsp-find-definition
	 "d" 'flycheck-list-errors
	 "gl" 'magit-log-all
	 "gb" 'magit-blame
	 "gs" 'magit-status
	 "gp" 'magit-pull
	 "gh" 'magit-log-buffer-file)
	(evil-mode t))

(use-package docker
	:ensure t
	:bind ("C-c d" . docker))

(use-package go-mode
	:ensure t
	:hook ((go-mode . lsp-deferred)))

(use-package typescript-mode
	:mode ("\\.ts\\'" "\\.tsx\\'")
	:hook (typescript-mode . lsp-deferred)
	:config
	(setq typescript-indent-level 2))


(use-package flycheck
	:ensure t
	:init (global-flycheck-mode))
	
(use-package centaur-tabs
	:ensure t
	:config
	(setq centaur-tabs-style "chamfer")
	(setq centaur-tabs-set-icons t)
	(setq centaur-tabs-set-bar 'under)
	(setq x-underline-at-descent-line t)
	(setq centaur-tabs-cycle-scope 'tabs)
	(setq centaur-tabs-set-close-button nil)
	(centaur-tabs-headline-match)
	(centaur-tabs-mode t)
	:hook (emacs-startup . centaur-tabs-mode)
  :bind
	("C-t" . 'centaur-tabs-counsel-switch-group)
	(:map evil-normal-state-map
				("g t" . centaur-tabs-forward)
				("g T" . centaur-tabs-backward)))

(use-package doom-themes
 	:ensure t
 	:config
 	(setq doom-themes-enable-bolt t
 				doom-themes-enable-italic t)
 	(load-theme 'doom-one t)) ;; doom-nord  doom-wilmersdorf  doom-city-lights

(use-package simple-modeline
	:ensure t
  :hook (after-init . simple-modeline-mode))

(use-package hideshow
	:defer t
  :diminish hs-minor-mode
  :hook (prog-mode  . hs-minor-mode))

;;(use-package yasnippet
;;	:ensure t
;;  :diminish yas-minor-mode
;;  :preface (defvar tmp/company-point nil)
;;  :config
;;  (yas-global-mode +1)
;;  (advice-add 'company-complete-common
;;              :before
;;              #'(lambda ()
;;                  (setq tmp/company-point (point))))
;;  (advice-add 'company-complete-common
;;              :after
;;              #'(lambda ()
;;                  (when (equal tmp/company-point (point))
;;                    (yas-expand)))))

(use-package company
	:ensure t
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
	:ensure t
  :hook (company-mode . company-box-mode))


(use-package org
	:ensure t
	:config
	:hook (prog-mode . yas-minor-mode))

;; HELP FUCNTIONS ========================================================================================================================================================================================================================================

(defun unique-shell ()
	"Create a new named shell buffer."
  (interactive)
  (call-interactively 'term)
  ;;(call-interactively 'shell)
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

(global-set-key (kbd "C-'") #'imenu-list-smart-toggle)

(define-key package-menu-mode-map (kbd "C-h") 'evil-window-left)
(define-key package-menu-mode-map (kbd "C-j") 'evil-window-down)
(define-key package-menu-mode-map (kbd "C-k") 'evil-window-up)
(define-key package-menu-mode-map (kbd "C-l") 'evil-window-right)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

(define-key evil-normal-state-map (kbd "C-c k") 'centaur-tabs--kill-this-buffer-dont-ask)

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

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode) ;; add icons to dired using all-the-icons-dired-mode

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

;;(setq lsp-gopls-codelens nil)

;; =======================================================================================================================================================================================================================================================



;;; init ends here
