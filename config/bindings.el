;;; Leader
(define-prefix-command 'my-leader-map)

(keymap-set evil-motion-state-map "," 'my-leader-map)
(keymap-set evil-normal-state-map "," 'my-leader-map)

(evil-define-key nil my-leader-map
  ;; add your bindings here:
  "a"  'ace-window
  "b"  'switch-to-buffer
  "p"  'tabspaces-project-switch-project-open-file
  "q"  'my/delete-buffer-or-workspace
  "cc" 'recenter-top-bottom
  "gf" 'magit-find-file
  "gs" 'magit-status
  "hv" 'helpful-variable
  "hf" 'helpful-function
  "hk" 'helpful-key
)

(define-key evil-normal-state-map (kbd "s-t") 'tabspaces-switch-or-create-workspace)
;(define-key evil-normal-state-map (kbd "C-s") 'tabspaces-project-switch-project-open-file)

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


(define-key evil-normal-state-map (kbd "s-}") 'tab-bar-switch-to-next-tab)
(define-key evil-normal-state-map (kbd "s-{") 'tab-bar-switch-to-prev-tab)

(define-key evil-normal-state-map (kbd "C-b") 'tabspaces-switch-to-buffer)

(after 'embark-autoloads
    (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
    (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
    (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump)))

