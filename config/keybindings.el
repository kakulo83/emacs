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

(define-key evil-insert-state-map (kbd "s-k") 'comint-clear-buffer)

(define-key evil-motion-state-map (kbd "C-z") nil)
(define-key evil-motion-state-map (kbd "RET") nil)
(define-key evil-motion-state-map (kbd "C-f") nil)
(define-key evil-normal-state-map (kbd "-") 'dired)
(define-key evil-normal-state-map (kbd "C-t") 'tab-bar-switch-to-tab)
(define-key evil-normal-state-map (kbd "C-s") 'switch-project-with-new-tab) ; 'projectile-switch-project)
(define-key evil-normal-state-map (kbd "C-p") 'project-find-file)
(define-key evil-normal-state-map (kbd "C-b") 'switch-to-buffer)
(define-key evil-normal-state-map (kbd "C-n") 'treemacs)
(define-key evil-normal-state-map (kbd "z-c") 'vimish-fold) ; 'hs-hide-block)
(define-key evil-normal-state-map (kbd "z-o") 'vimish-fold-delete) ;'hs-show-block)
(define-key evil-normal-state-map (kbd "C-c n") 'org-roam-capture)
(define-key evil-normal-state-map (kbd "/") 'consult-line)
(define-key evil-motion-state-map (kbd "n") 'isearch-repeat-forward)
(define-key evil-motion-state-map (kbd "N") 'isearch-repeat-backward)

(evil-define-key 'normal org-mode-map (kbd "C-j") 'evil-window-down)
(evil-define-key 'normal org-mode-map (kbd "C-k") 'evil-window-up)
(evil-define-key 'normal eshell-mode-map (kbd "C-n") 'treemacs)
(evil-define-key 'normal treemacs-mode-map (kbd "s") 'treemacs-visit-node-ace-horizontal-split)
(evil-define-key 'normal treemacs-mode-map (kbd "i") 'treemacs-visit-node-ace-vertical-split)
(evil-define-key 'normal vterm-mode-map (kbd "C-z") 'unique-shell)
(evil-define-key 'normal pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
(evil-define-key 'normal pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
(evil-define-key 'normal pdf-view-mode-map (kbd "d") 'pdf-view-next-page)
(evil-define-key 'normal pdf-view-mode-map (kbd "u") 'pdf-view-previous-page)
(evil-define-key 'normal treemacs-mode-map (kbd "a") 'treemacs-create-file)
(evil-define-key 'normal treemacs-mode-map (kbd "m") 'treemacs-move-file)
(evil-define-key 'normal treemacs-mode-map (kbd "d") 'treemacs-delete-file)
(evil-define-key 'normal treemacs-mode-map (kbd "r") 'treemacs-refresh)

(evil-ex-define-cmd "q" 'kill-this-buffer)
