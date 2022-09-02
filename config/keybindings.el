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
; Tab Related (define-key evil-normal-state-map (kbd "C-t") 'tab-bar-switch-to-tab)
(define-key evil-normal-state-map (kbd "C-s") 'switch-project-with-new-tab) ; 'projectile-switch-project)
(define-key evil-normal-state-map (kbd "C-p") 'project-find-file)
(define-key evil-normal-state-map (kbd "C-b") 'switch-to-buffer)
(define-key evil-normal-state-map (kbd "C-S-b") 'list-buffers)
(define-key evil-normal-state-map (kbd "C-n") 'treemacs)
(define-key evil-normal-state-map (kbd "C-c n") 'org-roam-capture)
(define-key evil-normal-state-map (kbd "/") 'consult-line)
(define-key evil-normal-state-map (kbd "*") 'isearch-forward-symbol-at-point)
(define-key evil-motion-state-map (kbd "n") 'isearch-repeat-forward)
(define-key evil-motion-state-map (kbd "N") 'isearch-repeat-backward)

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
;(define-key evil-normal-state-map "gd" 'lsp-bridge-find-def)
;(define-key evil-normal-state-map "gr" 'lsp-bridge-find-references)

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
(evil-define-key 'insert shell-mode-map (kbd "C-h") 'consult-history)

; TODO bind up/down arrow key to show previous/next shell command
;(evil-define-key 'insert shell-mode-map (kbd "C-up") 'comint-previous-input)
;(evil-define-key 'insert shell-mode-map (kbd "C-down") 'comint-next-input)

(evil-ex-define-cmd "q" 'kill-this-buffer)
(evil-ex-define-cmd "noh" 'lazy-highlight-cleanup)
