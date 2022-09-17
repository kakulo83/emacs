(add-hook 'eshell-mode-hook
          (lambda ()
						(define-key eshell-mode-map (kbd "C-h") #'robert/eshell-history)))

;; Try to use vterm-mode-map and keybind "C-h" to history completion function
;; have to investigate whehther vterm exposes its history


;; Insert at prompt only on eshell
(add-hook 'eshell-mode-hook
					'(lambda ()
						 (define-key evil-normal-state-local-map (kbd "i") (lambda () (interactive) (evil-goto-line) (evil-append-line nil)))))

(add-hook 'vterm-mode-hook
					'(lambda ()
						 (define-key evil-normal-state-local-map (kbd "i") (lambda () (interactive) (evil-goto-line) (evil-append-line nil)))))

(add-hook 'pdf-view-mode-hook
					(lambda ()
						(set (make-local-variable 'evil-normal-state-cursor) (list nil))
						(evil-set-initial-state 'pdf-view-mode 'normal)
						(pdf-view-midnight-minor-mode)))

(add-hook 'outline-mode-hook
					(lambda ()
						(evil-set-initial-state 'outline-mode 'normal)))

(advice-add 'org-babel-do-key-sequence-in-edit-buffer
            :around #'evil-org-insert-state-in-edit-buffer)

(add-hook 'emacs-startup-hook 'toggle-frame-maximized) ;; Make fullscreen

(add-hook 'vterm-mode-hook (lambda() (linum-mode 0)))
(add-hook 'org-mode-hook (lambda() (linum-mode 0)))
(add-hook 'sql-mode-hook 'sqlformat-on-save-mode)
(add-hook 'eshell-mode-hook (lambda()
															(linum-mode 0)))

(add-hook 'sql-interactive-mode-hook
	  (lambda ()
	    (sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
	    (sql-set-product-feature 'postgres :prompt-cont-regexp
                           "^[-[:alnum:]_]*[-(][#>] ")))

(add-hook 'dired-mode-hook 'auto-revert-mode)


