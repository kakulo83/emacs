;;; package --- Summary -*- lexical-binding: t;-*-
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
		'("34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"
			 "b30ab3b30e70f4350dad6bfe27366d573ace2190cc405c619bd5e602110c6e0c"
			 "e3b2bad7b781a968692759ad12cb6552bc39d7057762eefaf168dbe604ce3a4b"
			 "98b4ef49c451350c28a8c20c35c4d2def5d0b8e5abbc962da498c423598a1cdd"
			 default))
 '(org-bullets-bullet-list '("◉" "○" "✸" "◉🌿"))
 '(package-selected-packages
		'(ace-window all-the-icons-dired all-the-icons-ibuffer
			 balanced-windows bug-hunter cape copilot copilot-chat corfu
			 dape dired-sidebar diredfl disaster doom-modeline
			 embark-consult eshell-toggle evil-collection
			 exec-path-from-shell exunit flycheck format-all gcmh
			 git-timemachine helpful hide-mode-line highlight-symbol hydra
			 inf-elixir inf-ruby kind-icon lsp-tailwindcss lsp-ui marginalia
			 modus-themes multi-vterm nodejs-repl nvm nyan-mode olivetti
			 orderless org-bullets org-download org-drill org-roam-ui
			 prodigy pyvenv restclient rspec-mode symbols-outline tabspaces
			 undo-tree vertico vterm-toggle wgrep yasnippet-capf
			 yasnippet-snippets))
 '(package-vc-selected-packages
		'((lsp-tailwindcss :url
				"https://github.com/merrickluo/lsp-tailwindcss" :branch
				"master")
			 (copilot-chat :url "https://github.com/chep/copilot-chat.el"
				 :branch "master")
			 (copilot :url "https://github.com/copilot-emacs/copilot.el"
				 :branch "main")
			 (miasma-theme :url "https://github.com/daut/miasma-theme.el"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-subtree-depth-1-face ((t nil)))
 '(dired-subtree-depth-2-face ((t nil)))
 '(dired-subtree-depth-3-face ((t nil)))
 '(fixed-pitch ((t (:family "Fira Code Retina" :height 160))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#ffffff" :font "ETBembo" :height 4.0 :underline nil))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#ffffff" :font "ETBembo" :height 2.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#ffffff" :font "ETBembo" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#ffffff" :font "ETBembo" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#ffffff" :font "ETBembo" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#ffffff" :font "ETBembo"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#ffffff" :font "ETBembo"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#ffffff" :font "ETBembo"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#ffffff" :font "ETBembo"))))
 '(org-link ((t (:foreground "deep sky blue" :underline t))))
 '(org-meta-line ((t (:inherit fixed-pitch :height 0.8))))
 '(org-property-value ((t (:inherit fixed-pitch))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(tab-bar-tab ((t (:inherit tab-bar :foreground "orange"))))
 '(tab-bar-tab-inactive ((t (:box nil))))
 '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin)))))

(provide 'custom)
;;; custom.el ends here

