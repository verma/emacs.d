;; such emacs init
;;


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)

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

;; visual bell glitches fixes.
(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)

(use-package powerline
  :ensure t
  :config
  (powerline-center-theme))
  

(use-package evil
  :ensure t
  :config (evil-mode 1))

(use-package paredit
  :ensure t)

(use-package evil-paredit
  :ensure t)

(use-package cider
  :ensure t
  :defer t
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

(use-package racket-mode
  :ensure t
  :config
  (add-hook 'racket-mode-hook
            (lambda ()
              (paredit-mode)
              (evil-paredit-mode)
              (cond
               ((string-equal system-type "gnu/linux")
                (progn
                  (setq-default racket-racket-program "/usr/bin/racket")
                  (setq-default racket-raco-program "/usr/bin/raco")))
               ((string-equal system-type "darwin")
                (progn
                  (setq-default racket-racket-program "/Applications/Racket v6.8/bin/racket")
                  (setq-default racket-raco-program "/Applications/Racket v6.8/bin/raco")))))))

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
  :config (global-company-mode))

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

(use-package sass-mode
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x G" . magit-status))
  :config
  (setq vc-handled-backends ()))

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-materia t))
  

(defun disable-anti-aliasing ()
  (interactive)
  (if (eq system-type 'darwin)
      (setq mac-allow-anti-aliasing nil)))

(defun set-preferred-font ()
  (interactive)
  (set-default-font
   (cond
    ((eq system-type 'gnu/linux)
     "Ubuntu Mono 14")
    ((eq system-type 'darwin)
     "Ubuntu Mono 16"))))

(defun set-preferred-settings ()
  (interactive)
  (set-preferred-font)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (setq-default line-spacing 6))

(set-preferred-settings)

