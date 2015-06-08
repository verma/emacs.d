;; better defaults initialization, along with our own better defalts
;;
(provide 'init-better-defaults)

(require 'better-defaults)
(require 'hl-line)

;; have lines highlighted at all times
(global-hl-line-mode)

;; make sure we have company mode enabled
(add-hook 'after-init-hook 'global-company-mode)

;; make sure new-line always indents the next line correctly
;;
(global-set-key "\C-m" 'newline-and-indent)

