;; my own stuffs
;;
(provide 'init-mine)

;; Some utility functions to do stuff
(defun fix-mode-line ()
  (set-face-attribute 'mode-line nil :family "Andale Mono" :height 100))

(defun set-day-time ()
  (interactive)
  (load-file "~/.emacs.d/themes/tomorrow/color-theme-tomorrow.el")
  (color-theme-tomorrow)
  (fix-mode-line))


(defun set-night-time ()
  (interactive)
  (load-file "~/.emacs.d/themes/atom-one-dark-theme.el")
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
  (set-frame-font (font-spec :family "Monaco"
                             :size 14
                             :antialias nil))
  (fix-mode-line))

(defun stress-relief-mode ()
  (interactive)
  (setq mac-allow-anti-aliasing t)
  (set-frame-font (font-spec :family "PragmataPro"
                             :size 16
                             :antialias t))
  (fix-mode-line))

(defun many-noob-mode ()
  (interactive)
  (setq mac-allow-anti-aliasing t)
  (set-frame-font (font-spec :family "Consolas"
                             :size 18
                             :antialias t))
  (fix-mode-line))

(defun many-noob-pro-mode ()
  (interactive)
  (setq mac-allow-anti-aliasing t)
  (set-frame-font (font-spec :family "Source Code Pro"
                             :size 18
                             :antialias t))
  (fix-mode-line))

(defun lispy-spacing ()
  (interactive)
  (setq-default line-spacing 3))

(defun blind-mode ()
  (interactive)
  (lispy-spacing)
  (many-noob-mode))


