;;; package --- Summary -*- lexical-binding: t;-*-
;;; Commentary:
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-bullets-bullet-list '("◉" "○" "✸" "◉"))
 '(package-selected-packages nil)
 '(package-vc-selected-packages
    '((copilot-chat :url "https://github.com/chep/copilot-chat.el" :branch
	"master")
       (copilot :url "https://github.com/copilot-emacs/copilot.el"
	 :branch "main"))))
;(set-face-attribute 'dired-perm-write nil :foreground "dark red")
(add-hook 'emacs-startup-hook
          (lambda ()
            (custom-set-faces
	      '(dired-perm-write ((t (:foreground "dark red" :underline t))))
	      )))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-perm-write ((t (:foreground "dark red" :underline t))))
 '(fixed-pitch ((t (:family "Fira Code Retina" :height 160))))
 '(line-number ((t (:inherit default :background "#ffffff00" :foreground "#989898"))))
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

