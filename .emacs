;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(shell-command-to-string "xset r rate 180 30")

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/" "~/.emacs.d/themes/alect-themes/")
(add-to-list 'load-path "~/.emacs.d/lisp/")


;;(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
(setq same-window-regexps '("."))
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq x-select-enable-clipboard t)
(setq org-bullets-bullet-list '("◉" "✿" "◇" "○" "~"))
(setq org-hide-emphasis-markers t)
(setq org-src-fontify-natively t)
(setq org-return-follows-link t)
(setq helm-grep-ag-command "rg --smart-case --no-heading --line-number %s %s %s")
(setq evil-want-C-u-scroll t)
(setq tab-width 2)
(setq diredp-hide-details-initially-flag nil)
(setq ring-bell-function 'ignore)
(setq neo-smart-open t)
(setq neo-show-hidden-files t)
(setq projectile-enable-caching t)
(setq word-wrap nil)
(setq term-buffer-maximum-size 0)
(setq org-return-follows-link t)

(set-default-font "Inconsolata Bold")
(set-face-attribute 'default nil :height 80)
(set-default 'truncate-lines -1)

(require 'yasnippet)
(require 'org-bullets)
(require 'package)
(require 'nyan-mode)
(require 'evil)
(require 'neotree)
(require 'evil-magit)
(require 'dired+)
(require 'general)
(require 'origami)

(show-paren-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(yas-global-mode 1)
(nyan-mode 1)
(evil-mode 1)
(projectile-mode 1)
(origami-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("0dd717ae0704f14f39cf6da5b3a8ff11a768f21586936d46e3d3ffcac28d1400" "f0021feeaa66dfe9d4f58c17a612c9b5e200c17e3b8297bdde899b6296cb53fd" "ed91d4e59412defda16b551eb705213773531f30eb95b69319ecd142fab118ca" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(dired-use-ls-dired t)
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(fci-rule-color "#222222")
 '(frame-brackground-mode (quote dark))
 '(fringe-mode 10 nil (fringe))
 '(gnus-logo-colors (quote ("#528d8d" "#c0c0c0")))
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")))
 '(linum-format " %6d ")
 '(main-line-color1 "#222232")
 '(main-line-color2 "#333343")
 '(package-selected-packages
   (quote
    (origami general weechat alect-themes cider multi-term helm-projectile dired+ evil-magit neotree evil nyan-mode magit avy helm org yasnippet)))
 '(powerline-color1 "#222232")
 '(powerline-color2 "#333343")
 '(vc-annotate-background "#222222")
 '(vc-annotate-color-map
   (quote
    ((20 . "#fa5151")
     (40 . "#ea3838")
     (60 . "#f8ffa0")
     (80 . "#e8e815")
     (100 . "#fe8b04")
     (120 . "#e5c900")
     (140 . "#32cd32")
     (160 . "#8ce096")
     (180 . "#7fb07f")
     (200 . "#3cb370")
     (220 . "#099709")
     (240 . "#2fdbde")
     (260 . "#1fb3b3")
     (280 . "#8cf1f1")
     (300 . "#94bff3")
     (320 . "#62b6ea")
     (340 . "#30a5f5")
     (360 . "#e353b9"))))
 '(vc-annotate-very-old-color "#e353b9"))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-directory ((t (:inherit font-lock-function-name-face :foreground "deep sky blue"))))
 '(dired-header ((t (:inherit font-lock-type-face :foreground "yellow"))))
 '(dired-perm-write ((t (:inherit font-lock-comment-delimiter-face :foreground "gold"))))
 '(diredp-dir-name ((t (:foreground "deep sky blue"))))
 '(diredp-dir-priv ((t (:foreground "white smoke"))))
 '(diredp-exec-priv ((t (:foreground "deep sky blue"))))
 '(diredp-file-name ((t (:foreground "thistle4"))))
 '(diredp-file-suffix ((t nil)))
 '(diredp-no-priv ((t nil)))
 '(diredp-number ((t (:foreground "gold2"))))
 '(diredp-read-priv ((t (:foreground "gold3"))))
 '(diredp-write-priv ((t (:foreground "gold1"))))
 '(font-lock-string-face ((t (:foreground "lime green"))))
 '(helm-buffer-directory ((t (:background "black" :foreground "royal blue"))))
 '(helm-buffer-file ((t (:foreground "white"))))
 '(helm-ff-directory ((t (:background "black" :foreground "cornflower blue"))))
 '(helm-ff-file ((t (:background "black" :foreground "light steel blue"))))
 '(helm-selection ((t (:background "gray39" :distant-foreground "black"))))
 '(helm-source-header ((t (:foreground "gold" :weight bold :height 1.3 :family "Sans Serif"))))
 '(org-block ((t (:inherit shadow :background "black" :foreground "white"))))
 '(org-level-1 ((t (:inherit outline-1 :foreground "white" :weight bold :height 1.75))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "dark magenta" :weight bold :height 1.3))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "#4682b4" :weight bold :height 1.2)))))




(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "|") nil)
  (define-key evil-motion-state-map (kbd "-") nil)
  (define-key evil-motion-state-map (kbd ":") 'helm-M-x)
  (define-key evil-normal-state-map (kbd "\C-p") 'projectile-find-file))

(eval-after-load "term"
  '(progn
     ;; ensure that scrolling doesn't break on output
     (setq term-scroll-to-bottom-on-output t)))


(global-set-key (kbd "M-x")  'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h")  'windmove-left)
(global-set-key (kbd "C-j")  'windmove-down)
(global-set-key (kbd "C-k")  'windmove-up)
(global-set-key (kbd "C-l")  'windmove-right)
(global-set-key (kbd "C--")  'split-window-below)
(global-set-key (kbd "C-\\") 'split-window-right)
(global-set-key (kbd "M-l") 'helm-buffers-list)
(global-set-key (kbd "C-o") 'previous-buffer)


(evil-define-minor-mode-key 'normal 'neotree-mode (kbd "RET") 'neotree-enter)

(define-key evil-normal-state-map (kbd "-") 'open-pwd-dired)
(define-key evil-normal-state-map (kbd "RET") 'neotree-enter)
(define-key evil-normal-state-map (kbd "C-n") 'neotree-toggle)
(define-key evil-normal-state-map (kbd "C-f") 'helm-projectile-grep)
(define-key lisp-interaction-mode-map (kbd "C-h") 'windmove-left)
(define-key lisp-interaction-mode-map (kbd "C-j") 'windmove-down)
(define-key lisp-interaction-mode-map (kbd "C-k") 'windmove-up)
(define-key lisp-interaction-mode-map (kbd "C-l") 'windmove-right)

(define-key dired-mode-map (kbd "C-k") 'windmove-up)
(define-key dired-mode-map (kbd "C-h") 'windmove-left)
(define-key dired-mode-map (kbd "M-l") 'helm-buffers-list)
(define-key dired-mode-map (kbd "C--") 'dired-display-below)
(define-key dired-mode-map (kbd "C-\\") 'dired-display-right)



(general-define-key :keymaps 'neotree-mode-map
  :states '(normal emacs)
  "a" 'neotree-create-node)
(general-define-key :keymaps 'neotree-mode-map
  :states '(normal emacs)
  "d" 'neotree-delete-node)
(general-define-key :keymaps 'neotree-mode-map
  :states '(normal emacs)
  "I" 'neotree-hidden-file-toggle)
(general-define-key :keymaps 'neotree-mode-map
  :states '(normal emacs)
  "C" 'neotree-change-root)
(general-define-key :keymaps 'neotree-mode-map
  :states '(normal emacs)
  "i" 'neotree-enter-horizontal-split)
(general-define-key :keymaps 'neotree-mode-map
  :states '(normal emacs)
  "s" 'neotree-enter-vertical-split)



(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))

(add-hook 'org-mode-hook
	  (lambda ()
	    (add-to-list 'org-emphasis-alist '("*" (:foreground "red")))
	    (add-to-list 'org-emphasis-alist '("=" (:background "" :foreground "gold")))
	    (define-key org-mode-map (kbd "C-j") 'windmove-down)
	    (define-key org-mode-map (kbd "C-k") 'windmove-up)
	    ))

(add-hook 'term-mode-hook
	  (lambda ()
	    (define-key term-raw-map (kbd "C-h") 'windmove-left)
	    (define-key term-raw-map (kbd "C-j") 'windmove-down)
	    (define-key term-raw-map (kbd "C-k") 'windmove-up)
	    (define-key term-raw-map (kbd "C-l") 'windmove-right)
	    (define-key term-raw-map (kbd "C--") 'split-window-below)
	    (define-key term-raw-map (kbd "C-\\") 'split-window-right)))
	  
(add-hook 'find-file-hook 
			   (lambda ()
			     (origami-mode 1)
			     (linum-mode 1)))

(add-hook 'cider-repl-mode-hook
	  (lambda ()
	    (define-key cider-repl-mode-map (kbd "s-k") 'cider-repl-clear-buffer)
	    (define-key cider-repl-mode-map (kbd "s-r") 'cider-refresh)
	    (define-key cider-repl-mode-map (kbd "C-j") 'windmove-down)))


(evil-define-key 'normal dired-mode-map (kbd ":") 'helm-M-x)

(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting"
  (other-window 1))

(defun open-pwd-dired ()
  (interactive)
    (dired default-directory))


(defun notes (subject)
  "Function to quickly open/create new org/notes file"
  (interactive "ssubject: ")
  (find-file (format "~/Dropbox/Notes/%s/%s.org" subject subject)))


(defun my-run-term ()
    (interactive)
    (require 'multi-term)
    (command-execute 'multi-term)
    (setq-default truncate-lines nil)
    (if (not (boundp 'term-number))
        (defvar term-number 1 "term index in the current emacs session") )
    (rename-buffer (concat "Term " (int-to-string term-number)))
    (setq term-number (+ 1 term-number)))
(global-set-key (kbd "C-x t") 'my-run-term) ;; mappe sur C-T



(defun my-display-buffer-below (buffer alist)
"Doc-string."
  (let (
      (window
        (cond
          ((get-buffer-window buffer (selected-frame)))
          ((window-in-direction 'below))
          (t
            (split-window (selected-window) nil 'below)))))
    (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)
    window))

(defun my-display-buffer-above (buffer alist)
"Doc-string."
  (let (
      (window
        (cond
          ((get-buffer-window buffer (selected-frame)))
          ((window-in-direction 'above))
          (t
            (split-window (selected-window) nil 'above)))))
    (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)
    window))

(defun my-display-buffer-left (buffer alist)
"Doc-string."
  (let (
      (window
        (cond
          ((get-buffer-window buffer (selected-frame)))
          ((window-in-direction 'left))
          (t
            (split-window (selected-window) nil 'left)))))
    (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)
    window))

(defun my-display-buffer-right (buffer alist)
"Doc-string."
  (let (
      (window
        (cond
          ((get-buffer-window buffer (selected-frame)))
          ((window-in-direction 'right))
          (t
            (split-window (selected-window) nil 'right)))))
    (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)
    window))

(defun dired-display-above ()
"Doc-string."
(interactive)
  (let* (
      (file-or-dir (dired-get-file-for-visit))
      (buffer (find-file-noselect file-or-dir)))
    (my-display-buffer-above buffer nil)))

(defun dired-display-below ()
"Doc-string."
(interactive)
  (let* (
      (file-or-dir (dired-get-file-for-visit))
      (buffer (find-file-noselect file-or-dir)))
    (my-display-buffer-below buffer nil)))

(defun dired-display-left ()
"Doc-string."
(interactive)
  (let* (
      (file-or-dir (dired-get-file-for-visit))
      (buffer (find-file-noselect file-or-dir)))
    (my-display-buffer-left buffer nil)))

(defun dired-display-right ()
"Doc-string."
(interactive)
  (let* (
      (file-or-dir (dired-get-file-for-visit))
      (buffer (find-file-noselect file-or-dir)))
    (my-display-buffer-right buffer nil)))
