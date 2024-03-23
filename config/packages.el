;; :init executes code BEFORE a package is loaded
;; :config executes code AFTER a package is loaded

(use-package bug-hunter)


(use-package evil
  :config
  (evil-mode 1))


(use-package magit)


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
  	  (?? aw-show-dispatch-help)
  	  )))


(use-package balanced-windows
  :config
  (balanced-windows-mode))


(use-package ef-themes
  :config
  (load-theme 'ef-duo-dark))


(use-package tabspaces
 :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  ;; Scopes buffer list to only workspace available
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-include-buffers '("*scratch*"))
  ;; Removes buffers from the default tabspace 
  (tabspaces-remove-to-default nil)
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t))


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
  (setq embark-indicator #'embark-mixed-indicator)
  (setq embark-verbose-indicator-display-action
        '(display-buffer-at-bottom
  	  (window-height . fit-window-to-buffer)))
  :bind
  (("M-o" . embark-act)))
