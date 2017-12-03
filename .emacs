
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
;;(add-to-list 'default-frame-alist '(alpha . (85 . 50)))

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/" "~/.emacs.d/themes/alect-themes/")
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
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
(setq linum-format "%d ")
(setq org-latex-create-formula-image-program 'dvipng)
(setq cider-auto-select-error-buffer nil)


(set-default-font "Inconsolata")
(set-default 'truncate-lines -1)

(require 'yasnippet)
(require 'org-bullets)
(require 'package)
(require 'nyan-mode)
(require 'evil)
(require 'neotree)
(require 'evil-magit)
(require 'dired+)
(require 'multi-term)

(show-paren-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(yas-global-mode 1)
(nyan-mode 1)
(evil-mode 1)
(projectile-mode 1)


(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("0dd717ae0704f14f39cf6da5b3a8ff11a768f21586936d46e3d3ffcac28d1400" "f0021feeaa66dfe9d4f58c17a612c9b5e200c17e3b8297bdde899b6296cb53fd" "ed91d4e59412defda16b551eb705213773531f30eb95b69319ecd142fab118ca" default)))
 '(fringe-mode 0 nil (fringe))
 '(linum-format " %6d ")
 '(main-line-color1 "#222232")
 '(main-line-color2 "#333343")
 '(package-selected-packages
   (quote
    (exec-path-from-shell auctex multi-term projectile helm-ag cider rjsx-mode dired+ evil-magit neotree evil nyan-mode magit avy helm org yasnippet)))
 '(powerline-color1 "#222232")
 '(powerline-color2 "#333343"))


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
 '(helm-buffer-directory ((t (:background "black" :foreground "royal blue"))))
 '(helm-buffer-file ((t (:foreground "white"))))
 '(helm-ff-directory ((t (:background "black" :foreground "cornflower blue"))))
 '(helm-ff-file ((t (:background "black" :foreground "light steel blue"))))
 '(helm-selection ((t (:background "gray39" :distant-foreground "black"))))
 '(helm-source-header ((t (:foreground "gold" :weight bold :height 1.3 :family "Sans Serif"))))
 '(org-block ((t (:inherit shadow :background "gray10" :foreground "MediumPurple3"))))
 '(org-block-begin-line ((t (:inherit org-meta-line :background "gray10" :foreground "gray16"))))
 '(org-level-1 ((t (:inherit outline-1 :foreground "white" :weight bold :height 1.75))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "dark magenta" :weight bold :height 1.3))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "#4682b4" :weight bold :height 1.2))))
 '(vertical-border ((((type tty)) (:inherit mode-line-inactive :background "Black" :foreground "Black")))))


(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd ":") 'helm-M-x)
  (define-key evil-normal-state-map (kbd "\C-p") 'projectile-find-file)) ;; 'helm-find-files))

(global-set-key (kbd "\C-p") 'projectile-find-file)
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

(global-unset-key (kbd "s-k"))

(define-key evil-normal-state-map (kbd "RET") 'neotree-enter)
(define-key evil-normal-state-map (kbd "C-n") 'neotree-toggle)
;;(define-key evil-insert-state-map (kbd "kj") 'evil-force-normal-state)

(add-hook 'find-file-hook 'linum-mode)

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
	  

(define-key lisp-interaction-mode-map (kbd "C-h") 'windmove-left)
(define-key lisp-interaction-mode-map (kbd "C-j") 'windmove-down)
(define-key lisp-interaction-mode-map (kbd "C-k") 'windmove-up)
(define-key lisp-interaction-mode-map (kbd "C-l") 'windmove-right)


(define-key dired-mode-map (kbd "C-k") 'windmove-up)
(define-key dired-mode-map (kbd "C-h") 'windmove-left)
(define-key dired-mode-map (kbd "M-l") 'helm-buffers-list)


(add-hook 'cider-repl-mode-hook
	  (lambda ()
	    (define-key cider-repl-mode-map (kbd "s-k") 'cider-repl-clear-buffer)
	    (define-key cider-repl-mode-map (kbd "s-r") 'cider-refresh)
	    (define-key cider-repl-mode-map (kbd "C-j") 'windmove-down)))


(add-hook 'cider--debug-mode-hook 'my-cider-debug-toggle-insert-state)

(evil-define-key 'normal dired-mode-map (kbd ":") 'helm-M-x)

(defun my-cider-debug-toggle-insert-state ()
  (if cider--debug-mode    ;; Checks if you're entering the debugger   
      (evil-insert-state)  ;; If so, turn on evil-insert-state
    (evil-normal-state)))  ;; Otherwise, turn on normal-state



(defun notes (subject)
  "Function to quickly open/create new org/notes file"
  (interactive "ssubject: ")
  (find-file (format "~/Dropbox/Notes/%s/%s.org" subject subject)))

(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting"
  (other-window 1))

(shell-command-to-string "xset r rate 180 30")


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

(defun neotree-project-dir-toggle ()
  "Open NeoTree using the project root, using find-file-in-project, or the current buffer directory."
  (interactive)
  (let ((project-dir
         (ignore-errors
           ;;; Pick one: projectile or find-file-in-project
           ; (projectile-project-root)
           (ffip-project-root)
           ))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
        (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
            (neotree-dir project-dir))
        (if file-name
            (neotree-find file-name))))))

;;(eval-when-compile (require 'cl-lib))
;;
;;(defun split-window-func-with-other-buffer (split-function)
;;  (lexical-let ((s-f split-function))
;;    (lambda ()
;;      (interactive)
;;      (funcall s-f)
;;      (set-window-buffer (next-window) (other-buffer)
;;	(other-window 1)))))
;;
;;(global-set-key (kbd "C--") (split-window-func-with-other-buffer 'split-window-vertically))
;;(global-set-key (kbd "C-\\") (split-window-func-with-other-buffer 'split-window-horizontally))
