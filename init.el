;;NOTE
;;  - To find init.el bugs I installed:  https://github.com/Malabarba/elisp-bug-hunter
;;  - when there's a bug run M-x bug-hunter-init-file RET and enter 'e'

(require 'org)
(org-babel-load-file
 (expand-file-name "configuration.org"
                   user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 '(ivy-rich marginalia pdf-tools orderless treemacs-icons-dired lsp-ui evil-leader popwin treemacs-evil treemacs counsel tron-legacy-theme alect-themes org-bullets go-mode projectile lsp-mode org-roam org evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
