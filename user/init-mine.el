;; my own stuffs
;;
(provide 'init-mine)

;; Some utility functions to do stuff
(defun fix-mode-line ()
  (set-face-attribute 'mode-line nil :family "PragmataPro" :height 130))

(defun set-day-time ()
  (interactive)
  (load-file "~/.emacs.d/themes/tomorrow/color-theme-tomorrow.el")
  (color-theme-tomorrow)
  (fix-mode-line))


(defun set-night-time ()
  (interactive)
  (load-file "~/.emacs.d/themes/noctilux-definitions.el")
  (load-file "~/.emacs.d/themes/noctilux-theme.el")
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

(defun lispy-spacing ()
  (interactive)
  (setq-default line-spacing 3))


