(setq use-package-always-ensure t)
(setq warning-minimum-level :error)
(setq use-dialog-box nil)

;(setq sql-postgres-login-params
;      '((user :default "robertcarter")
;        (database :default "db/onboardiq_dev")
;        (server :default "localhost")
;        (port :default 5432)))


(setq lexical-binding t)
(setq evil-want-fine-undo 'yes)
(setq initial-scratch-message "")
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      ; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
      ) ;; increase garbage-collection threshold to 100mb from 8kb
(setq all-the-icons-dired-monochrome nil)
(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

(fset 'yes-or-no-p 'y-or-n-p)  ;; use 'y' and 'n' for 'yes' and 'no'

(setq completion-ignore-case t)


(setq inhibit-splash-screen t) ;; Disable splash screen

(setq initial-major-mode (quote fundamental-mode)) ;; Disable scratch screen

(setq ring-bell-function 'ignore) ;; silence alert chimes

(setq scroll-step 1
      scroll-conservatively  10000) ;; Smoother scrolling with smaller steps

(setq-default tab-width 2) ;; Set tab space to 2

(setq org-hidden-keywords '(title))

(setq-default explicit-shell-file-name "/bin/zsh")

(setq lsp-enable-links nil)

(setq show-paren-style 'parenthesis)

(setq eshell-prompt-regexp "^[^λ]+ λ ")
(setq eshell-prompt-function
			(lambda ()
				(concat
				 (propertize (concat (eshell/pwd)) 'face `(:foreground "black" :background "turquoise1"))
				 (propertize "" 'face `(:foreground "turquoise1" :background "gray34"))
				 (if (magit-get-current-branch)
						 (concat
							(propertize "  " 'face `(:foreground "turquoise1" :background "gray34"))
							(propertize (magit-get-current-branch) 'face `(:foreground "turquoise1" :background "gray34"))
							(propertize "" 'face `(:foreground "gray34"))
						 ))
				 (propertize "\n")
         (propertize "❱ " 'face `(:foreground "white"))
				 )))

(cond
  ((string-equal system-type "darwin")
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))))

(setq frame-title-format '(""))

(setq comint-input-ignoredups t) ;; remove duplicate commands from history

(global-auto-revert-mode 1) ;; Automatically revert buffers for changed files

(global-linum-mode t) ;; Show number lines

(show-paren-mode 1)
