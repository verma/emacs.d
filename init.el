;; Maximum emacs configuration


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)

;;; Code:

(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)


;; general
(setq inhibit-startup-message t) ;; no startup message
(global-hl-line-mode) ;; highlight current line
(setq exec-path (append exec-path '("/usr/local/bin")))

(setq backup-by-copying t
      backup-directory-alist
      '(("." . "~/.saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; start with a somewhat decent size window
(when window-system
  (set-frame-size (selected-frame) 150 50))

;; visual bell glitches fixes.
(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)

(use-package powerline
  :ensure t
  :config
  (powerline-center-theme))

(use-package auto-highlight-mode
  :ensure t)

(use-package evil
  :ensure t
  :config (evil-mode 1))

(use-package paredit
  :ensure t)

(use-package evil-paredit
  :ensure t)

(use-package cider
  :ensure t
  :config
  (setq cider-auto-select-error-buffer nil)
  (setq cider-auto-jump-to-error nil)
  (setq cider-auto-select-test-report-buffer nil))

(defvar om-methods
  (list 'render
        'render-state
        'init-state
        'will-mount
        'did-mount
        'did-update
        'should-update
        'will-receive-props
        'will-update
        'display-name
        'will-unmount
        'ident
        'query
        'query-param)
  "Methods that should receive sexy clojure indents.")

(defun customize-clojure-indents ()
  (dolist (v om-methods)
    (put-clojure-indent v 'defun)))

(use-package clojure-mode
  :ensure t
  :defer t
  :config (add-hook 'clojure-mode-hook
                    (lambda ()
                      (paredit-mode)
                      (evil-paredit-mode)
                      (customize-clojure-indents))))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package flycheck-clojure
  :ensure t
  :config
  (eval-after-load 'flycheck '(flycheck-clojure-setup)))

(use-package flycheck-pos-tip
  :ensure t
  :config
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(use-package racket-mode
  :ensure t
  :config
  (add-hook 'racket-mode-hook
            (lambda ()
	      (custom-set-faces
            ;; racket-mode
            '(racket-paren-face ((((background dark)) (face-attribute 'default :foreground))
                                 (((background light)) (face-attribute 'default :foreground)))))
	      (paredit-mode)
              (evil-paredit-mode)
              (cond
               ((string-equal system-type "gnu/linux")
                (progn
                  (setq-default racket-racket-program "/usr/bin/racket")
                  (setq-default racket-raco-program "/usr/bin/raco")))
               ((string-equal system-type "darwin")
                (progn
                  (setq-default racket-racket-program "/usr/local/bin/racket")
                  (setq-default racket-raco-program "/usr/local/bin/raco")))))))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files))
  :config
  (helm-mode 1)

  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t))

(use-package helm-clojuredocs
  :ensure t
  :bind (("C-c C-w" . helm-clojuredocs-at-point)))

(use-package clj-refactor
  :ensure t
  :defer t
  :config
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (setq cljr-warn-on-eval nil))

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :diminish company-mode)

(use-package projectile
  :ensure t
  :defer t
  :config
  (projectile-global-mode)

  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action 'helm-projectile)
  (setq projectile-completion-system 'helm))

(use-package helm-projectile
  :ensure t
  :defer t)

(use-package ag
  :ensure t)

(use-package helm-ag
  :ensure t
  :bind (("C-x C-p" . helm-find-files)
         ("C-x C-p" . helm-projectile)
         ("C-x C-u" . helm-projectile-ag)))

(use-package dumb-jump
  :ensure t
  :defer t
  :bind (("C-c C-i" . dumb-jump-go)
         ("C-c i" . dumb-jump-back)))

(use-package js2-mode
  :ensure t
  :defer t)

(use-package web-mode
  :ensure t
  :defer t)

(use-package scss-mode
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x G" . magit-status))
  :config
  (setq vc-handled-backends ()))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'ace-window))

(use-package dracula-theme
  :ensure t)

(defun disable-anti-aliasing ()
  (interactive)
  (if (eq system-type 'darwin)
      (setq mac-allow-anti-aliasing nil)))

(defun proggy-mode ()
  (interactive)
  (set-default-font "ProggyCleanTT 16")
  (disable-anti-aliasing))

(defun set-preferred-font ()
  (interactive)
  (set-default-font
   (cond
    ((eq system-type 'gnu/linux)
     "Ubuntu Mono 12")
    ((eq system-type 'darwin)
     "Office Code Pro 15"))))

(defun set-preferred-settings ()
  (interactive)
  (set-preferred-font)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)

  (setq-default line-spacing 6)
  (load-theme 'dracula t))

(set-preferred-settings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (auto-highlight-symbol cmake-mode ace-window which-key color-theme-modern dockerfile-mode docker json-mode scribble-mode flycheck-elixir elixir-mode kotlin-mode leuven-theme graphql-mode github-theme yaml-mode web-mode use-package smex scss-mode sass-mode rust-mode racket-mode pug-mode powerline php-mode magit lfe-mode less-css-mode js2-mode helm-projectile helm-clojuredocs helm-ag go-mode git-gutter flycheck-rust fiplr evil-paredit erlang dumb-jump company clj-refactor better-defaults base16-theme ag))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
