(with-eval-after-load 'prog-mode (bind-key "C-f" #'consult-ripgrep))

(define-key package-menu-mode-map (kbd "C-h") 'evil-window-left)
(define-key package-menu-mode-map (kbd "C-j") 'evil-window-down)
(define-key package-menu-mode-map (kbd "C-k") 'evil-window-up)
(define-key package-menu-mode-map (kbd "C-l") 'evil-window-right)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
(define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)
(define-key evil-normal-state-map (kbd "TAB") nil)
(define-key evil-normal-state-map (kbd "s-}") 'persp-next)
(define-key evil-normal-state-map (kbd "s-{") 'persp-prev)
(define-key evil-normal-state-map (kbd "s-1") '(lambda ()(interactive) (tab-bar-select-tab 1)))
(define-key evil-normal-state-map (kbd "s-2") '(lambda ()(interactive) (tab-bar-select-tab 2)))
(define-key evil-normal-state-map (kbd "s-3") '(lambda ()(interactive) (tab-bar-select-tab 3)))
(define-key evil-normal-state-map (kbd "s-4") '(lambda ()(interactive) (tab-bar-select-tab 4)))
(define-key evil-normal-state-map (kbd "s-5") '(lambda ()(interactive) (tab-bar-select-tab 5)))

(with-eval-after-load "evil-maps"
	(define-key evil-normal-state-map (kbd "TAB") nil))

(define-key evil-insert-state-map (kbd "s-k") 'comint-clear-buffer)

(define-key evil-motion-state-map (kbd "C-z") nil)
(define-key evil-motion-state-map (kbd "RET") nil)
(define-key evil-motion-state-map (kbd "C-f") nil)
(define-key evil-normal-state-map (kbd "-") 'dired)
(define-key evil-normal-state-map (kbd "C-s") 'projectile-persp-switch-project)
(define-key evil-normal-state-map (kbd "C-p") 'project-find-file)
(define-key evil-normal-state-map (kbd "C-b") 'persp-switch-to-buffer*) ;'consult-project-buffer)
(define-key evil-normal-state-map (kbd "C-S-b") 'consult-buffer)
(define-key evil-normal-state-map (kbd "C-n") 'dired-sidebar-toggle-sidebar)
(define-key evil-normal-state-map (kbd "C-c n") 'org-roam-capture)
(define-key evil-normal-state-map (kbd "/") 'consult-line)
(define-key evil-normal-state-map (kbd "*") 'isearch-forward-symbol-at-point)
(define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward) ; projectile-previous-project-buffer
(define-key evil-motion-state-map (kbd "C-i") 'better-jumper-jump-forward)  ; projectile-next-project-buffer
(define-key evil-normal-state-map (kbd "n") 'isearch-repeat-forward)
(define-key evil-normal-state-map (kbd "N") 'isearch-repeat-backward)
(define-key evil-normal-state-map (kbd "C-'") 'consult-imenu)
(define-key evil-normal-state-map "zc" nil)
(define-key evil-normal-state-map "zo" nil)
(define-key evil-normal-state-map "zM" nil)
(define-key evil-normal-state-map "zR" nil)
(define-key evil-normal-state-map "zc" 'yafolding-toggle-element)
(define-key evil-normal-state-map "zo" 'yafolding-toggle-element)
(define-key evil-normal-state-map "zM" 'yafolding-toggle-all)
(define-key evil-normal-state-map "zR" 'yafolding-toggle-all)
(define-key evil-normal-state-map "gd" 'xref-find-definitions)
(define-key evil-normal-state-map "gr" 'xref-find-references)
(define-key evil-normal-state-map "gt" 'persp-next)
(define-key evil-normal-state-map "gT" 'persp-prev)
(define-key evil-normal-state-map ",k" 'robert/quick-kill-process)
(define-key evil-normal-state-map ",d" 'robert/drill-by-topic)
(define-key evil-normal-state-map "?" 'consult-eglot-symbols)

(evil-define-key 'normal org-mode-map (kbd "C-j") 'evil-window-down)
(evil-define-key 'normal org-mode-map (kbd "C-k") 'evil-window-up)
(evil-define-key 'normal vterm-mode-map (kbd "C-z") 'unique-shell)
(evil-define-key 'normal vterm-mode-map (kbd "C-d") 'evil-scroll-down)
(evil-define-key 'normal pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
(evil-define-key 'normal pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
(evil-define-key 'normal pdf-view-mode-map (kbd "d") 'pdf-view-next-page)
(evil-define-key 'normal pdf-view-mode-map (kbd "u") 'pdf-view-previous-page)
(evil-define-key 'insert eshell-mode-map (kbd "C-h") 'consult-history)

(evil-define-key 'motion grep-mode-map (kbd "C-p") 'project-find-file)
(evil-define-key 'motion grep-mode-map (kbd "C-b") 'persp-switch-to-buffer)
(evil-define-key 'motion grep-mode-map (kbd "/") 'consult-line)

(evil-ex-define-cmd "q" 'persp-kill-buffer*)
(evil-ex-define-cmd "noh" 'lazy-highlight-cleanup)

(evil-define-key 'motion eshell-mode-map (kbd "0") 'eshell-bol)


(evil-define-key 'normal grep-mode-map (kbd ",a")  'ace-window)
(define-key grep-mode-map (kbd "C-h") 'evil-window-left)
(define-key grep-mode-map (kbd "C-j") 'evil-window-down)
(define-key grep-mode-map (kbd "C-k") 'evil-window-up)
(define-key grep-mode-map (kbd "C-l") 'evil-window-right)

(define-key grep-mode-map (kbd "s-1") '(lambda () (interactive) (tab-bar-select-tab 1)))
(define-key grep-mode-map (kbd "s-2") '(lambda () (interactive) (tab-bar-select-tab 2)))
(define-key grep-mode-map (kbd "s-3") '(lambda () (interactive) (tab-bar-select-tab 3)))
(define-key grep-mode-map (kbd "s-4") '(lambda () (interactive) (tab-bar-select-tab 4)))
(define-key grep-mode-map (kbd "s-5") '(lambda () (interactive) (tab-bar-select-tab 5)))

(define-key grep-mode-map (kbd "C-b") 'persp-switch-to-buffer)

(global-set-key (kbd "C-z") #'unique-vterm-shell)
;(global-set-key (kbd "C-z") #'unique-eshell)
