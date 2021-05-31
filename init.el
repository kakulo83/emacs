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
;;;
;;; TODO
;;;  - convenient git-blame
;;;  - setup Tramp mode to interact with Staging/Production
;;;  - add keybinding to move to next/previous diagnostic error
;;;
;;; NOTE
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
	 '(counsel simple-modeline vimish-fold imenu-list lsp-ui typescript-mode lsp-mode go-mode bug-hunter use-package all-the-icons-dired tron-legacy-theme evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
	(define-key evil-motion-state-map (kbd "C-z") nil)
	(define-key evil-normal-state-map (kbd "C-s") 'projectile-switch-project)
	(define-key evil-normal-state-map (kbd "C-n") 'neotree-toggle)
	(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)
	(define-key evil-normal-state-map (kbd "C-b") 'ivy-switch-buffer)
	(define-key evil-normal-state-map (kbd "-") 'dired)
	(define-key evil-normal-state-map (kbd "C-t") 'centaur-tabs-counsel-switch-group)
	(define-key evil-normal-state-map (kbd "C-f") 'counsel-rg)
	(define-key evil-normal-state-map (kbd "z-c") 'hs-hide-block)
	(define-key evil-normal-state-map (kbd "z-o") 'hs-show-block)
  (evil-mode))

(use-package evil-collection
	:after evil
	:ensure t
	:config
	(evil-collection-init))

(use-package all-the-icons)

(use-package org-bullets
  :init
  (setq inhibit-compacting-font-caches t)
  (add-hook 'org-mode-hook #'org-bullets-mode))

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
  :init
  (setq lsp-keymap-prefix "C-c l")
	(setq lsp-headerline-breadcrumb-enable nil)
  :hook (go-mode . lsp-deferred)
	:hook (typescript-mode . lsp-deferred)
	:hook (ruby-mode . lsp-deferred)
	:config
  :commands lsp)

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
	 "gl" 'magit-log
	 "gb" 'magit-blame
	 "gs" 'magit-status
	 "gp" 'magit-pull
	 "gh" 'magit-log-buffer-file)
	(evil-mode t))

(use-package docker
	:ensure t
	:bind ("C-c d" . docker))

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
	(load-theme 'doom-wilmersdorf t)
	(doom-themes-neotree-config))

(use-package simple-modeline
	:ensure t
  :hook (after-init . simple-modeline-mode))

(use-package hideshow
	:defer t
  :diminish hs-minor-mode
  :hook (prog-mode  . hs-minor-mode))

;; HELP FUCNTIONS ========================================================================================================================================================================================================================================

(defun unique-shell ()
	"Create a new named shell buffer."
  (interactive)
  (call-interactively 'shell)
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

;; MODES =================================================================================================================================================================================================================================================

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
;; NOTE: lsp in typescript is not watching all the directories for OneSignal because that is over 2k directories and
;; would likely slow Emacs down
;; You can configure this warning with the 'lsp-enable-file-watchers' and 'lsp-file-watch-threshold' variables
;; there is also 'lsp-file-watch-ignored'

(show-paren-mode 1)

;; =======================================================================================================================================================================================================================================================

;; Disable Toolbar
(tool-bar-mode -1)

;; Make fullscreen
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

;; Disable splash screen
(setq inhibit-splash-screen t)

;; Disable scratch screen
(setq initial-major-mode (quote fundamental-mode))

;; silence alert chimes
(setq ring-bell-function 'ignore)

;; Enable syntax highlighting
(global-font-lock-mode 1)

;; Don't wrap lines
(set-default 'truncate-lines t)

;; Don't show scroll bars
(toggle-scroll-bar -1)

;; Show number lines
(global-linum-mode t)

;; add icons to dired using all-the-icons-dired-mode
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; Smoother scrolling with smaller steps
(setq scroll-step            1
      scroll-conservatively  10000)

;; Focus cursor in new split buffers
(defadvice split-window (after move-point-to-new-window activate)
  "Move the point to the newly created window after splitting."
  (other-window 1))

;; Set tab space to 2
(setq-default tab-width 2)

;;; init ends here
