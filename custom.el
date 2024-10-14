;;; package --- Summary -*- lexical-binding: t;-*-
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-bullets-bullet-list '("◉" "○" "✸" "◉🌿"))
 '(package-selected-packages
    '(ace-window all-the-icons-dired all-the-icons-ibuffer
       balanced-windows bug-hunter cape copilot copilot-chat corfu
       dape dired-sidebar diredfl disaster doom-modeline doom-themes
       ef-themes embark-consult envrc eshell-toggle evil-collection
       exec-path-from-shell exunit flycheck-eglot format-all
       frame-local gcmh git-timemachine helpful hide-mode-line
       highlight-symbol hydra-posframe inf-elixir inf-ruby kind-icon
       load-env-vars magit marginalia minimap modus-themes multi-vterm
       nano-theme nodejs-repl nvm nyan-mode olivetti orderless
       org-bullets org-download org-drill org-roam-ui package-lint
       pythonic pyvenv quelpa-use-package restclient symbols-outline
       tabspaces undo-tree use-package-ensure-system-package
       vertico-posframe vterm-toggle wgrep yasnippet-snippets))
 '(package-vc-selected-packages
    '((copilot-chat :url "https://github.com/chep/copilot-chat.el" :branch
	"master")
       (copilot :url "https://github.com/copilot-emacs/copilot.el"
	 :branch "main"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Fira Code Retina" :height 160))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "Black" :font "ETBembo" :height 4.0 :underline nil))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "Black" :font "ETBembo" :height 2.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "Black" :font "ETBembo" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "Black" :font "ETBembo" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "Black" :font "ETBembo" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "Black" :font "ETBembo"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "Black" :font "ETBembo"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "Black" :font "ETBembo"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "Black" :font "ETBembo"))))
 '(org-link ((t (:foreground "deep sky blue" :underline t))))
 '(org-meta-line ((t (:inherit fixed-pitch :height 0.8))))
 '(org-property-value ((t (:inherit fixed-pitch))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin)))))

(provide 'custom)
;;; custom.el ends here

