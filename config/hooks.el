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

(add-hook 'sql-mode-hook 'sqlformat-on-save-mode)

(add-hook 'sql-interactive-mode-hook
	  (lambda ()
	    (sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
	    (sql-set-product-feature 'postgres :prompt-cont-regexp
                           "^[-[:alnum:]_]*[-(][#>] ")))

(add-hook 'dired-mode-hook 'auto-revert-mode)

(add-hook 'evil-mode 'electric-pair-mode)

(advice-add #'corfu-insert :after #'corfu-send-shell)

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local corfu-auto nil)
            (corfu-mode)))

(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'dired-sidebar-mode-hook (lambda () (display-line-numbers-mode -1)))
