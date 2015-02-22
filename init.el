(setq inhibit-startup-message t)
(global-font-lock-mode t)
(menu-bar-mode 0)
(setq make-backup-files nil)

;; set tabs right
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq visible-bell 1)

;; Set Linum-Mode on
(global-linum-mode t)

;; Linum-Mode and add space after the number
(setq linum-format "%4d")

(setq find-file-visit-truename t)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/vendor/emacs-powerline")

(defvar my-packages '(better-defaults
                      clojure-mode
                      projectile
                      evil
                      evil-leader
                      evil-paredit
                      ido-vertical-mode
                      rainbow-delimiters
                      expand-region
                      js2-mode
                      clj-refactor
                      yasnippet
                      smex
                      less-css-mode
                      ample-theme
                      web-mode
					  fiplr
                      omnisharp
                      csharp-mode
                      company
                      flycheck
                      markdown-mode
                      cider))


(global-evil-leader-mode)
(evil-leader/set-leader ",")

(setq fiplr-root-markers '(".git" ".svn"))
(setq fiplr-ignored-globs '((directories (".git" ".svn" ".repl" "out" "target"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))

(add-to-list 'load-path
             "~/.emacs.d/vendor"
             "~/.emacs.d/themes")

(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-mode)) ; set js2-mode as major mode for javascript


(require 'evil)
(require 'evil-leader)
(require 'evil-paredit)
(require 'hl-line)
(require 'fiplr)
(require 'ido)
(require 'ido-vertical-mode)
(require 'rainbow-delimiters)
(require 'powerline)
(require 'expand-region)
(require 'clj-refactor)
(require 'yasnippet)
(require 'less-css-mode)
(require 'smex)
(require 'web-mode)

; initialize smex and bind keys
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


; Configure web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

; Setup modeline/powerline

(ido-mode t)
(ido-vertical-mode t)
(evil-mode t)                ; have evil mode on all the time
(paredit-mode t)
(pending-delete-mode t)                 ; Replace contents when typing on selection with expand-region
(yas-global-mode 1)                     ; enable snippets

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; turn on paredit when clojure is being edited
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'evil-paredit-mode)
(add-hook 'cider-repl-mode-hook 'enable-paredit-mode)

(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)

;; some scrolling customizations
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(global-set-key (kbd "C-=") 'er/expand-region)

(setq-default cursor-type 'bar)

(evil-leader/set-key
  "W" 'paredit-wrap-round
  "w[" 'paredit-wrap-square
  "w{" 'paredit-wrap-curly)

;; some refactor setup
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               ;; keybindings go here
                               (cljr-add-keybindings-with-modifier "C-s-")))

(projectile-global-mode)

;; much nice cursor colors for evil mode
;;
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

(global-hl-line-mode)

;; Configure csharp mode
(add-hook 'csharp-mode-hook (lambda ()
                                 (c-set-offset 'brace-list-intro 0)
                                 (c-set-offset 'brace-list-open 0)))

;; Make sure company mode is always available
(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))


;; Some utility functions to do stuff
(defun fix-mode-line ()
  (set-face-attribute 'mode-line nil :family "PragmataPro" :height 140))


(defun set-day-time ()
  (interactive)
  (load-file "~/.emacs.d/themes/tomorrow/color-theme-tomorrow.el")
  (color-theme-tomorrow)
  (fix-mode-line))


(defun set-night-time ()
  (interactive)
  (load-file "~/.emacs.d/themes/spacegray-theme.el")
  (fix-mode-line))

(defun hacker-mode ()
  (interactive)
  (setq mac-allow-anti-aliasing nil)
  (set-frame-font (font-spec :family "Andale Mono"
                             :size 14
                             :antialias nil))
  (fix-mode-line))

(defun noob-mode ()
  (interactive)
  (setq mac-allow-anti-aliasing t)
  (set-frame-font (font-spec :family "LiberationMono"
                             :size 15
                             :antialias nil))
  (fix-mode-line))

;; Always change to the code directory on the platform
(defvar startup-dirs '("~/Projects"))

(dolist (d startup-dirs)
  (when (file-exists-p d)
    (setq default-directory d)))

; Always start in noob mode
(noob-mode)

; Always start with the night mode
(set-night-time)

