(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
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

(set-face-bold-p 'bold nil)

(set-language-environment "UTF-8")

(set-default-coding-systems 'utf-8-unix)

; M-x describe-font
(set-frame-font "Roboto Mono:pixelsize=12:weight=light")
;(set-frame-font "Hack Nerd Font:pixelsize=10:weight=medium:slant=normal:width=normal:spacing=0:scalable=true 10" nil t)
;(set-frame-font "Inconsolata:pixelsize=12:weight=medium:slant=normal:width=normal:spacing=100:scalable=true 12" nil t)

(set-face-attribute 'default nil :height 100)

(global-font-lock-mode 1) ;; Enable syntax highlighting

(set-default 'truncate-lines t) ;; Don't wrap lines

(set-cursor-color "#00FFFF")


;(load-theme 'tron-legacy t)
;(load-theme 'granger  t)       ;; graham  fogus  granger
;(load-theme 'modus-vivendi t)  ;; modus-operandi    modus-vivendi
;(load-theme 'planet t)
;(load-theme 'solarized-iceberg-dark t)
(load-theme 'doom-outrun-electric t) ;; doom-nord  doom-wilmersdorf  doom-city-lights  doom-sourcerer  doom-outrun-electric  doom-vibrant  doom-nord-aurora  doom-Iosvkem
;(load-theme 'nano-dark)         ;; nano-light
