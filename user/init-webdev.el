;; init-webdev
;;

(provide 'init-webdev)

(require 'js2-mode)
(require 'web-mode)


;; js2 mode for all javascript files
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; web-mode for react jsx and html files
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

