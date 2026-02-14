;;; package --- Summary -*- lexical-binding: t;-*-
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(gptel-api-key "")
 '(package-selected-packages
		'(ace-window aio all-the-icons-ibuffer better-jumper bug-hunter cape
			 catppuccin-theme cherry-blossom-theme
			 color-theme-sanityinc-tomorrow consult-xref-stack corfu
			 deferred dired-sidebar disaster doom-modeline
			 doom-two-tone-themes eca ef-themes
			 eglot-signature-eldoc-talkative eldoc-box embark-consult
			 eshell-git-prompt eshell-toggle evil-collection evil-surround
			 exec-path-from-shell exunit flycheck-eglot format-all gcmh
			 git-timemachine go-playground gptel-quick gruvbox-theme helpful
			 hide-mode-line highlight-symbol ht hydra iceberg-theme
			 inf-elixir inf-ruby jtsx kind-icon magit marginalia
			 modus-themes monokai-pro-theme multi-vterm names nano-theme
			 nerd-icons-dired nodejs-repl nord-theme nordic-night-theme nvm
			 nyan-mode orderless org-alert org-download org-drill org-recur
			 org-roam-ui org-superstar outline-indent polymode popper
			 prodigy request restclient-jq rspec-mode shell-maker
			 south-theme spinner symbols-outline tabspaces treesit-auto
			 undo-tree vertico-posframe vterm-toggle wgrep yasnippet-capf
			 yasnippet-snippets))
 '(package-vc-selected-packages
		'((go-playground-cli :url
				"https://github.com/emacsmirror/go-playground-cli" :branch
				"master")
			 (gptel-quick :url "https://github.com/karthink/gptel-quick")
			 (south-theme :url "https://github.com/SophieBosio/south"
				 :branch "main")
			 (claude-code-ide :url
				 "https://github.com/manzaltu/claude-code-ide.el")
			 (doom-two-tone-themes :url
				 "https://github.com/eliraz-refael/doom-two-tone-themes"
				 :branch "master")
			 (copilot :url "https://github.com/copilot-emacs/copilot.el"
				 :branch "main")
			 (fleury-theme :url
				 "https://github.com/ShamsParvezArka/fleury-theme.el" :branch
				 "main")
			 (copilot-chat :url "https://github.com/chep/copilot-chat.el"
				 :branch "master")
			 (consult-xref-stack :url
				 "https://github.com/brett-lempereur/consult-xref-stack"
				 :branch "main")
			 (tsx-mode :url
				 "https://github.com/orzechowskid/tsx-mode.el.git" :branch
				 "emacs30")
			 (tron-theme :url
				 "https://github.com/paul-jewell/Emacs-Tron-Legacy-Theme"
				 :branch "master")
			 (tron :url
				 "https://github.com/paul-jewell/Emacs-Tron-Legacy-Theme"
				 :branch "master")
			 (ts-fold :url "https://github.com/emacs-tree-sitter/ts-fold"
				 :branch "master")))
 '(window-divider-default-bottom-width 1)
 '(window-divider-default-right-width 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Fira Code Retina" :height 160))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "deep sky blue" :underline t))))
 '(org-meta-line ((t (:inherit fixed-pitch :height 0.8))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(tab-bar-tab ((t (:inherit tab-bar))))
 '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin)))))

(provide 'custom)
;;; custom.el ends here

