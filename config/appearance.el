;;; package --- Summary
;;; Code:
;;; Commentary:
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;'(default ((t (:background nil))))
 '(blamer-face ((t :foreground "#7a88cf" :background nil :height 140 :italic t)))
 '(italic ((t (:foreground "white" :slant italic))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "Hack Nerd Font Bold" :height 6.0 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "Hack Nerd Font Bold" :height 2.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "Hack Nerd Font Bold" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "Hack Nerd Font Bold" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "Hack Nerd Font Bold" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "Hack Nerd Font Bold"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "Hack Nerd Font Bold"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "Hack Nerd Font Bold"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#B0CCDC" :font "Hack Nerd Font Bold")))))


(tool-bar-mode -1) ;; Disable Toolbar

(tab-bar-mode -1)

(toggle-scroll-bar -1) ;; Don't show scroll bars

(set-face-bold 'bold nil)

(set-language-environment "UTF-8")

(set-default-coding-systems 'utf-8-unix)

(global-font-lock-mode 1) ;; Enable syntax highlighting

(set-default 'truncate-lines t) ;; Don't wrap lines

(global-display-line-numbers-mode 1)

; Give all windows a 3 pixel border
(setq-default left-margin-width 3 right-margin-width 3) ; Define new widths.
(set-window-buffer nil (current-buffer)) ; Use them now.

; M-x describe-font
(set-frame-font "JetBrains Mono:pixelsize=12")
;(set-frame-font "Roboto Mono:pixelsize=12:weight=light")
;(set-frame-font "Roboto Mono:pixelsize=12:weight=regular:slant=normal:width=normal:spacing=100:scalable=true")
;(set-frame-font "Hack Nerd Font:pixelsize=14:weight=light:slant=normal:width=normal:spacing=0:scalable=true 10" nil t)

;(set-frame-font "Inconsolata:pixelsize=14:weight=medium:slant=normal:width=normal:spacing=100:scalable=true 12" nil t)
;(set-frame-font "Inconsolata Nerd Font:pixelsize=12:weight=Light")

;(load-theme 'tron-legacy t)
;(load-theme 'graham t)       ; graham  fogus  granger
;(load-theme 'planet t)
;(load-theme 'doom-sourcerer t) ; doom-nord  doom-wilmersdorf  doom-city-lights  doom-sourcerer  doom-outrun-electric  doom-vibrant  doom-nord-aurora  doom-Iosvkem  doom-moonlight
;(load-theme 'nano-dark t)         ; nano-light
;(load-theme 'modus-vivendi t) ; modus-operandi    modus-vivendi
;(load-theme 'nord t)
;(load-theme 'afternoon t)
;(load-theme 'badwolf t)
(load-theme  'ef-duo-dark t) ; ef-duo-dark  ef-deuteranopia-light  ef-deuteranopia-dark  ef-maris-light   ef-elea-light  ef-winter

(provide 'appearance)
;;; appearance.el ends here

