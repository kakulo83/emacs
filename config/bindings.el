;;; Leader
(define-prefix-command 'my-leader-map)

(keymap-set evil-motion-state-map "," 'my-leader-map)
(keymap-set evil-normal-state-map "," 'my-leader-map)

(evil-define-key nil my-leader-map
    ;; add your bindings here:
    "`"  'vterm-toggle
    "a"  'ace-window
    "b"  'consult-bookmark
    "f"  'avy-goto-char-2
    "p"  'tabspaces-project-switch-project-open-file
    "q"  'my/delete-buffer-or-workspace
    "y"  'consult-yank-from-kill-ring
    "cc" 'recenter-top-bottom
    "cp" 'copy-filepath-to-clipboard
    "gb" 'magit-blame
    "gH" 'git-timemachine
    "gl" 'magit-log-buffer-file
    "gL" 'magit-log-all
    "gf" 'magit-find-file
    "gs" 'magit-status
    "hv" 'helpful-variable
    "hf" 'helpful-function
    "hk" 'helpful-key
)

(define-key evil-normal-state-map (kbd "C-f") 'consult-ripgrep)

(define-key evil-normal-state-map (kbd "s-t") 'tabspaces-switch-or-create-workspace)

(define-key package-menu-mode-map (kbd "C-h") 'evil-window-left)
(define-key package-menu-mode-map (kbd "C-j") 'evil-window-down)
(define-key package-menu-mode-map (kbd "C-k") 'evil-window-up)
(define-key package-menu-mode-map (kbd "C-l") 'evil-window-right)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

(define-key evil-normal-state-map (kbd "C-n") 'dired-sidebar-toggle-sidebar)

(define-key evil-normal-state-map (kbd "C-p") 'project-find-file)

(define-key evil-normal-state-map (kbd "/") 'consult-line)
(define-key evil-normal-state-map (kbd "C-'") 'consult-imenu)

(define-key evil-normal-state-map (kbd "s-}") 'tab-bar-switch-to-next-tab)
(define-key evil-normal-state-map (kbd "s-{") 'tab-bar-switch-to-prev-tab)

(define-key evil-normal-state-map (kbd "C-b") 'tabspaces-switch-to-buffer)

(after 'embark
    (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
    (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
    (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))
    (define-key embark-region-map "f" #'fill-region)
    (define-key embark-identifier-map (kbd "o") (my/embark-ace-action xref-find-definitions)))


(define-key package-menu-mode-map (kbd "C-h") 'evil-window-left)
(define-key package-menu-mode-map (kbd "C-j") 'evil-window-down)
(define-key package-menu-mode-map (kbd "C-k") 'evil-window-up)
(define-key package-menu-mode-map (kbd "C-l") 'evil-window-right)


(define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
(define-key evil-motion-state-map (kbd "C-i") 'better-jumper-jump-forward)

(evil-define-key 'motion eshell-mode-map (kbd "0") 'eshell-bol)

(define-key evil-normal-state-map (kbd "-") 'dired)

(define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
(define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)
