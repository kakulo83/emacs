;;; -*- lexical-binding: t -*-
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
  "b"  'hydra-bookmark/body ; 'consult-bookmark
  "c"  'hydra-ai/body ;'gptel
  "d"  'robert/drill-by-topic
  "e"  'hydra-flycheck/body
  "f"  'avy-goto-char-2
  "g"  'hydra-vc/body
  "i"  'hydra-repl/body
  "k"  'robert/quick-kill-process
  "n"  'robert/open-notes-dired-in-tab
  "p"  'tabspaces-project-switch-project-open-file
  "q"  'my/delete-buffer-or-workspace
  "r"  'hydra-register/body
  "s"  'hydra-snippets/body
  "w"  'hydra-window-utils/body
  "x"  'eval-region
  "y"  'consult-yank-from-kill-ring
  "hv" 'helpful-variable
  "hf" 'helpful-function
  "hk" 'helpful-key
)

(define-key package-menu-mode-map (kbd "C-h") 'evil-window-left)
(define-key package-menu-mode-map (kbd "C-j") 'evil-window-down)
(define-key package-menu-mode-map (kbd "C-k") 'evil-window-up)
(define-key package-menu-mode-map (kbd "C-l") 'evil-window-right)

(define-key evil-normal-state-map (kbd "f") 'avy-goto-char-2)
(define-key evil-normal-state-map (kbd "M-s-<left>") 'tab-bar-move-tab-backward)
(define-key evil-normal-state-map (kbd "M-s-<right>") 'tab-bar-move-tab)
(define-key evil-normal-state-map (kbd "C-f") 'consult-ripgrep)
(define-key evil-normal-state-map (kbd "s-t") 'tabspaces-switch-or-create-workspace)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-n") 'dired-sidebar-toggle-sidebar)
(define-key evil-normal-state-map (kbd "C-p") 'project-find-file)
(define-key evil-normal-state-map (kbd "/") 'consult-line)
(define-key evil-normal-state-map (kbd "C-'") 'symbols-outline-show);; 'consult-imenu)
(define-key evil-normal-state-map (kbd "s-}") 'tab-bar-switch-to-next-tab)
(define-key evil-normal-state-map (kbd "s-{") 'tab-bar-switch-to-prev-tab)
(define-key evil-normal-state-map (kbd "C-b") 'tabspaces-switch-to-buffer);; 'consult-buffer tabspaces-switch-to-buffer)
(define-key evil-normal-state-map (kbd "C-S-B") 'ibuffer)
(define-key evil-normal-state-map (kbd "-") 'dired-jump)
(define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
(define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)
(define-key evil-normal-state-map "gd" 'xref-find-definitions)
(define-key evil-normal-state-map "gr" 'xref-find-references)
(define-key evil-normal-state-map "*" 'highlight-symbol)
(define-key evil-normal-state-map "n" 'highlight-symbol-next)
(define-key evil-normal-state-map "N" 'highlight-symbol-prev)
(define-key evil-normal-state-map (kbd "C-z") 'robert/unique-vterm-shell)
(define-key evil-normal-state-map (kbd "s-`") 'eshell)



(with-eval-after-load 'evil-maps
		(define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
		(define-key evil-motion-state-map(kbd "C-i") 'better-jumper-jump-forward))

(define-key evil-normal-state-map (kbd "s-1") #'(lambda ()(interactive) (tab-bar-select-tab 1)))
(define-key evil-normal-state-map (kbd "s-2") #'(lambda ()(interactive) (tab-bar-select-tab 2)))
(define-key evil-normal-state-map (kbd "s-3") #'(lambda ()(interactive) (tab-bar-select-tab 3)))
(define-key evil-normal-state-map (kbd "s-4") #'(lambda ()(interactive) (tab-bar-select-tab 4)))
(define-key evil-normal-state-map (kbd "s-5") #'(lambda ()(interactive) (tab-bar-select-tab 5)))
(define-key evil-normal-state-map (kbd "s-6") #'(lambda ()(interactive) (tab-bar-select-tab 6)))
(define-key evil-normal-state-map (kbd "s-7") #'(lambda ()(interactive) (tab-bar-select-tab 7)))
(define-key evil-normal-state-map (kbd "s-8") #'(lambda ()(interactive) (tab-bar-select-tab 8)))
(define-key evil-normal-state-map (kbd "s-9") #'(lambda ()(interactive) (tab-bar-select-tab 9)))
;(define-key evil-insert-state-map (kbd "C-n") 'cape-prefix-map)
(define-key evil-insert-state-map (kbd "S-<return>") #'copilot-accept-completion)
																				; copilot-accept-completion-by-line
																				; copilot-accept-completion-by-word
(define-key evil-insert-state-map (kbd "C-h") 'cape-history)

(after 'embark
  ;(define-key embark-general-map (kbd "d") #'robert/embark-clear-register)
  (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
  (define-key embark-file-map     (kbd "z") #'dired-do-compress)

  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))

	(keymap-set embark-general-map "?" #'gptel-quick)

  (define-key embark-region-map "f" #'fill-region)
  (define-key embark-region-map "r" #'gptel-rewrite)
  (define-key embark-region-map "b" #'vc-region-history)
  (define-key embark-region-map "c" #'robert/embark-org-roam-cut-to-new-note)

  (define-key embark-identifier-map (kbd "o") (my/embark-ace-action xref-find-definitions))
  (define-key embark-identifier-map "r" #'eglot-rename)
	(define-key embark-identifier-map "f" #'consult-ripgrep)
  (define-key embark-identifier-map (kbd "d") (my/embark-ace-action eldoc-print-current-symbol-info))
)

(define-key magit-status-mode-map (kbd "]]") 'magit-section-forward)
(define-key magit-status-mode-map (kbd "[[") 'magit-section-backward)

(define-key grep-mode-map (kbd "s-1") #'(lambda ()(interactive) (tab-bar-select-tab 1)))
(define-key grep-mode-map (kbd "s-2") #'(lambda ()(interactive) (tab-bar-select-tab 2)))
(define-key grep-mode-map (kbd "s-3") #'(lambda ()(interactive) (tab-bar-select-tab 3)))
(define-key grep-mode-map (kbd "s-4") #'(lambda ()(interactive) (tab-bar-select-tab 4)))
(define-key grep-mode-map (kbd "s-5") #'(lambda ()(interactive) (tab-bar-select-tab 5)))
(define-key grep-mode-map (kbd "s-6") #'(lambda ()(interactive) (tab-bar-select-tab 6)))
(define-key grep-mode-map (kbd "s-7") #'(lambda ()(interactive) (tab-bar-select-tab 7)))
(define-key grep-mode-map (kbd "s-8") #'(lambda ()(interactive) (tab-bar-select-tab 8)))
(define-key grep-mode-map (kbd "s-9") #'(lambda ()(interactive) (tab-bar-select-tab 9)))
(define-key evil-motion-state-map (kbd "RET") nil) ; allows other mode maps to override RET

(define-key vertico-map (kbd "TAB") 'vertico-insert)

(define-key compilation-mode-map (kbd "s-1") #'(lambda ()(interactive) (tab-bar-select-tab 1)))
(define-key compilation-mode-map (kbd "s-2") #'(lambda ()(interactive) (tab-bar-select-tab 2)))
(define-key compilation-mode-map (kbd "s-3") #'(lambda ()(interactive) (tab-bar-select-tab 3)))
(define-key compilation-mode-map (kbd "s-4") #'(lambda ()(interactive) (tab-bar-select-tab 4)))
(define-key compilation-mode-map (kbd "s-5") #'(lambda ()(interactive) (tab-bar-select-tab 5)))
(define-key compilation-mode-map (kbd "s-6") #'(lambda ()(interactive) (tab-bar-select-tab 6)))
(define-key compilation-mode-map (kbd "s-7") #'(lambda ()(interactive) (tab-bar-select-tab 7)))
(define-key compilation-mode-map (kbd "s-8") #'(lambda ()(interactive) (tab-bar-select-tab 8)))
(define-key compilation-mode-map (kbd "s-9") #'(lambda ()(interactive) (tab-bar-select-tab 9)))

(define-key compilation-mode-map (kbd "C-h") 'evil-window-left)
(define-key compilation-mode-map (kbd "C-j") 'evil-window-down)
(define-key compilation-mode-map (kbd "C-k") 'evil-window-up)
(define-key compilation-mode-map (kbd "C-l") 'evil-window-right)

(define-key vterm-mode-map (kbd "M-`") nil)

(evil-define-key 'motion eshell-mode-map (kbd "0") 'eshell-bol)
(evil-define-key 'normal eshell-mode-map (kbd "[[") 'eshell-previous-prompt)
(evil-define-key 'normal eshell-mode-map (kbd "]]") 'eshell-next-prompt)
(evil-define-key 'normal dired-mode-map (kbd "L") 'evil-window-bottom)
(evil-define-key 'normal dired-mode-map (kbd "H") 'evil-window-top)
(evil-define-key 'normal dired-mode-map (kbd "M") 'evil-window-middle)
(evil-define-key 'insert eshell-mode-map (kbd "C-h") 'consult-history)
(evil-define-key 'insert vterm-mode-map (kbd "C-h") 'vterm-completion)
(evil-define-key 'insert inf-elixir-mode-map (kbd "C-h") 'consult-history)
(evil-define-key 'normal flycheck-mode-map (kbd "C-o") 'better-jumper-jump-backward); 'previous-buffer)
(evil-define-key 'normal messages-buffer-mode-map (kbd "C-o") 'better-jumper-jump-backward);'previous-buffer)
(evil-define-key 'normal symbols-outline-mode-map (kbd "RET") 'symbols-outline-visit-and-quit)
(evil-define-key 'normal dired-sidebar-mode-map (kbd "-") 'dired-sidebar-up-directory)

;(evil-define-key 'normal ibuffer-mode-map (kbd (my-leader-map . "a")) 'ace-window)

(with-eval-after-load 'html-mode
	(define-key html-mode-map (kbd "M-o M-o") nil))

(evil-define-key 'normal prodigy-mode-map (kbd "s") 'prodigy-start)
(evil-define-key 'normal prodigy-mode-map (kbd "S") 'prodigy-stop)
(evil-define-key 'normal prodigy-mode-map (kbd "/") 'prodigy-add-name-filter)
(evil-define-key 'normal prodigy-mode-map (kbd "F") 'prodigy-clear-filters)
(evil-define-key 'normal prodigy-mode-map (kbd "q") 'quit-window)
(evil-define-key 'normal prodigy-mode-map (kbd "$") 'prodigy-display-process)

(global-set-key (kbd "C-c RET") 'gptel-send)

(global-set-key (kbd "s-l") 'comint-clear-buffer) ; added to clear python-shell buffer

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
  (define-key evil-motion-state-map (kbd "<C-i>") 'better-jumper-jump-forward))
(provide 'bindings)
;;; bindings.el ends here

