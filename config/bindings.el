;;; package --- Summary
;;; Commentary:
;;; Code:

(define-prefix-command 'my-leader-map)

(keymap-set evil-motion-state-map "," 'my-leader-map)
(keymap-set evil-normal-state-map "," 'my-leader-map)

(evil-define-key nil my-leader-map
  ;; add your bindings here:
  "`"  'vterm-toggle
  "a"  'ace-window
  "b"  'consult-bookmark
  "d"  'robert/drill-by-topic
  "f"  'avy-goto-char-2
  "k"  'robert/quick-kill-process
  "p"  'tabspaces-project-switch-project-open-file
  "q"  'my/delete-buffer-or-workspace
  "r"  'eval-region
  "y"  'consult-yank-from-kill-ring
  "z"  'hydra-zoom/body
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

(define-key package-menu-mode-map (kbd "C-h") 'evil-window-left)
(define-key package-menu-mode-map (kbd "C-j") 'evil-window-down)
(define-key package-menu-mode-map (kbd "C-k") 'evil-window-up)
(define-key package-menu-mode-map (kbd "C-l") 'evil-window-right)

(define-key evil-normal-state-map (kbd "C-f") 'consult-ripgrep)
(define-key evil-normal-state-map (kbd "s-t") 'tabspaces-switch-or-create-workspace)
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
(define-key evil-normal-state-map (kbd "C-S-B") 'list-buffers)
(define-key evil-normal-state-map (kbd "-") 'dired)
(define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
(define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)
(define-key evil-normal-state-map "gd" 'xref-find-definitions)
(define-key evil-normal-state-map "gr" 'xref-find-references)
(define-key evil-normal-state-map (kbd "s-1") #'(lambda ()(interactive) (tab-bar-select-tab 1)))
(define-key evil-normal-state-map (kbd "s-2") #'(lambda ()(interactive) (tab-bar-select-tab 2)))
(define-key evil-normal-state-map (kbd "s-3") #'(lambda ()(interactive) (tab-bar-select-tab 3)))
(define-key evil-normal-state-map (kbd "s-4") #'(lambda ()(interactive) (tab-bar-select-tab 4)))
(define-key evil-normal-state-map (kbd "s-5") #'(lambda ()(interactive) (tab-bar-select-tab 5)))
(define-key evil-normal-state-map (kbd "s-6") #'(lambda ()(interactive) (tab-bar-select-tab 6)))
(define-key evil-normal-state-map (kbd "s-7") #'(lambda ()(interactive) (tab-bar-select-tab 7)))
(define-key evil-normal-state-map (kbd "s-8") #'(lambda ()(interactive) (tab-bar-select-tab 8)))
(define-key evil-normal-state-map (kbd "s-9") #'(lambda ()(interactive) (tab-bar-select-tab 9)))
(define-key evil-insert-state-map (kbd "TAB") 'robert/tab)
(define-key evil-insert-state-map (kbd "S-<return>") #'copilot-accept-completion)

(after 'embark
  (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))
  (define-key embark-region-map "f" #'fill-region)
  (define-key embark-identifier-map (kbd "o") (my/embark-ace-action xref-find-definitions))
  (define-key embark-identifier-map "n" #'eglot-rename)
  (define-key embark-identifier-map "t" #'hydra-test-runner/body)
  )

(define-key evil-motion-state-map (kbd "RET") nil) ; allows other mode maps to override RET
(define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
(define-key evil-motion-state-map (kbd "C-i") 'better-jumper-jump-forward)

(define-key vertico-map (kbd "TAB") 'vertico-insert)

(evil-define-key 'motion eshell-mode-map (kbd "0") 'eshell-bol)
(evil-define-key 'normal dired-mode-map (kbd "L") 'evil-window-bottom)
(evil-define-key 'normal dired-mode-map (kbd "H") 'evil-window-top)
(evil-define-key 'normal dired-mode-map (kbd "M") 'evil-window-middle)
(evil-define-key 'insert eshell-mode-map (kbd "C-h") 'consult-history)
(evil-define-key 'insert vterm-mode-map (kbd "C-h") 'vterm-completion)

(provide 'bindings)
;;; bindings.el ends here

