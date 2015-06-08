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


;; the vendor scripts go into vendor directory and user ones go to, you guess it.
;; we declare them as globals here since some of the setup scripts need those
;; to require stuff correctly.  E.g. we don't want evil sitting in the vendor directory
;; but rather vendor/evil.
;;
(setq vendor-directory
      (expand-file-name "vendor" user-emacs-directory))

;; The same goes for user directory, we just have it here for completeness sake.
;;
(setq user-directory
      (expand-file-name "user" user-emacs-directory))

;; this configuration comes pre-packages with everything it needs.
;; Package managers are not allowed because they break what I expect out
;; of my editor and then I hate my life.
;;
(add-to-list 'load-path vendor-directory)

;; we don't really directly include anything, we just require our own configuration
;; which auto-requires everything it needs
;;
(add-to-list 'load-path user-directory)

;; Make sure themes are included too, or they'd feel left out and won't trip you out
;; as much
(add-to-list 'load-path
             (expand-file-name "themes" user-emacs-directory))

;; Initialize things
;;
(require 'init-better-defaults)
(require 'init-powerline)
(require 'init-projectile)
(require 'init-evil)
(require 'init-clojure-mode)
(require 'init-smex)
(require 'init-webdev)
(require 'init-fiplr)
(require 'init-mine)

; Always start with the night mode
(set-night-time)

