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

;;(define-key outline-mode-map (kbd "j") 'next-line)
;;(define-key outline-mode-map (kbd "k") 'previous-line)
;;(define-key outline-mode-map (kbd "C-l") 'evil-window-right)
;;(define-key outline-mode-map (kbd "C-n") 'pdf-outline-quit)

(evil-ex-define-cmd "q" 'kill-this-buffer)
