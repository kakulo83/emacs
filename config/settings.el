;;; package --- Summary
;;; Commentary:
;;; Code:
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
(evil-set-command-property 'xref-find-references :jump t)
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

(setq completion-ignored-extensions
    (append completion-ignored-extensions
        (quote
        ("~undo-tree~"))))

(setq inhibit-splash-screen t) ;; Disable splash screen

(setq initial-major-mode (quote fundamental-mode)) ;; Disable scratch screen

(setq ring-bell-function 'ignore) ;; silence alert chimes

(setq scroll-step 1
      scroll-conservatively  10000) ;; Smoother scrolling with smaller steps

(setq-default tab-width 2) ;; Set tab space to 2

(setq org-hidden-keywords '(title))

(setq-default explicit-shell-file-name "/bin/zsh")

(setq show-paren-style 'expression)

(cond
  ((string-equal system-type "darwin")
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))))

(setq frame-title-format '(""))

(setq comint-input-ignoredups t) ;; remove duplicate commands from history

(global-auto-revert-mode 1) ;; Automatically revert buffers for changed files

(show-paren-mode 1)

(global-so-long-mode 1)

(setq comint-scroll-show-maximum-output nil)

(setq js-indent-level 2)

(setq electric-pair-mode 1)

(setenv "PAGER" "cat")

(set-fringe-mode 0)

(setf dired-kill-when-opening-new-dired-buffer t)

(setq auto-revert-check-vc-info t)

(global-display-line-numbers-mode 1)

(setq linum-format "%d ")

(setq vc-follow-symlinks t)

(provide 'settings)
;;; settings.el ends here
