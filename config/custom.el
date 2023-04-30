;;; package --- Summary
;;; Commentary:
;;;
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("5604af9b6347ef57c1dda7abc0a2edfe19a2fc75a2d106006320eca39e1fce66" "d43860349c9f7a5b96a090ecf5f698ff23a8eb49cd1e5c8a83bb2068f24ea563" "de8f2d8b64627535871495d6fe65b7d0070c4a1eb51550ce258cd240ff9394b0" "db5b906ccc66db25ccd23fc531a213a1afb500d717125d526d8ff67df768f2fc" "e87f48ec4aebdca07bb865b90088eb28ae4b286ee8473aadb39213d361d0c45f" default))
 '(eglot-ignored-server-capabilities '(:hoverProvider))
 '(eshell-syntax-highlighting-global-mode t)
 '(package-selected-packages
	 '(perspective-tabs eshell-syntax-highlighting kind-icon all-the-icons-completion nyan-mode eshell-git-prompt python-pytest paredit blackout nano-theme git-timemachine magit focus lsp-ui lox-mode prettier-js perspective typescript-mode tsx-mode modus-themes wgrep imenu-list side-hustle lsp-pyright python-black iceberg-theme web-mode corfu material-theme yafolding yasnippet-snippets use-package undo-tree tron-legacy-theme tree-sitter-langs sublime-themes sqlformat rg restclient rake rainbow-delimiters projectile pdf-view-restore origami org-roam-ui org-drill org-download org-bullets orderless olivetti native-complete multi-vterm marginalia kubel inflections inf-ruby helpful go-mode flycheck exec-path-from-shell evil-vimish-fold evil-leader evil-collection enh-ruby-mode embark-consult doom-themes doom-modeline docker dired-sidebar dashboard bug-hunter balanced-windows auctex all-the-icons-dired)))
;;; custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(blamer-face ((t :foreground "#7a88cf" :background nil :height 140 :italic t)))
 '(italic ((t (:foreground "white" :slant italic))))
 '(line-number ((t (:inherit default :background "nil" :foreground "#989898"))))
 '(nano-modeline-active-name ((t (:inherit (nano-modeline-active bold) :foreground "White"))))
 '(nano-modeline-active-primary ((t (:inherit nano-modeline-active :foreground "systemGreenColor"))))
 '(nano-modeline-active-status-** ((t (:inherit nano-modeline-active :foreground "Magenta"))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "Hack Nerd Font Bold" :height 6.0 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "Hack Nerd Font Bold" :height 2.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "Hack Nerd Font Bold" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "Hack Nerd Font Bold" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "Hack Nerd Font Bold" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "Hack Nerd Font Bold"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "Hack Nerd Font Bold"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "Hack Nerd Font Bold"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "Hack Nerd Font Bold"))))
 '(tab-bar ((t (:inherit variable-pitch :background "nil"))))
 '(tab-bar-tab ((t (:inherit bold :background "blue" :box (:line-width (2 . -2) :color "blue")))))
 '(tab-bar-tab-group-current ((t (:inherit bold :box (:line-width (2 . -2) :color "#000000")))))
 '(tab-bar-tab-inactive ((t (:background "nil" :box nil))))
 '(tab-line ((t (:inherit modus-themes-ui-variable-pitch :height 0.95)))))
