;; such emacs init
;;

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))

; list the packages you want
(setq package-list
      '(better-defaults
        undo-tree
        evil
        paredit
        evil-paredit
        cider
        clojure-mode
        sass-mode
        less-css-mode
        git-gutter
        powerline
        smex
        fiplr
        clj-refactor
        js2-mode
        base16-theme
        web-mode
        yaml-mode))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; general
(setq inhibit-startup-message t) ;; no startup message
(global-hl-line-mode) ;; highlight current line
(setq exec-path (append exec-path '("/usr/local/bin")))

;; evil
(evil-mode 1)

;; fiplr
(setq fiplr-root-markers '(".git" ".svn"))
(setq fiplr-ignored-globs '((directories (".git" ".svn" ".repl" "out" "target" "node_modules"))
                            (files ("*.jpg" "*.png" "*.zip" "*~" ".*" "#*"))))

(global-set-key (kbd "C-x C-p") 'fiplr-find-file)

;; powerline
(powerline-center-theme)

;; clojure-mode
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'evil-paredit-mode)

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
        'will-unmount)
  "Methods that should receive sexy clojure indents.")

(defun customize-clojure-indents ()
  (dolist (v om-methods)
    (put-clojure-indent v 'defun)))

(add-hook 'clojure-mode-hook #'customize-clojure-indents)

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; clj-refactor
(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; font
(if (eq system-type 'darwin)
  (setq mac-allow-anti-aliasing t))

(set-face-attribute 'default nil :font "Consolas-10")
(set-frame-font "Consolas-10" nil t)
(setq-default lispy-spacing 3)


;; theme
(defun daytime-colors ()
  (interactive)
  (load-theme 'base16-flat-light t)
  (set-face-background 'hl-line "#ffffff")
  (set-face-foreground 'highlight nil))

(defun nighttime-colors ()
  (interactive)
  (load-theme 'base16-flat-dark t)
  (set-face-background 'hl-line "#333333")
  (set-face-foreground 'highlight nil))

(defun current-hour ()
  (nth 2 (decode-time)))

(defun set-time-based-theme ()
  (let ((hour (current-hour)))
    (if (< 7 hour 19) (daytime-colors) (nighttime-colors))))

(set-time-based-theme)

