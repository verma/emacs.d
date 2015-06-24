;; initialize evil mode
;;
(provide 'init-cider)

(add-to-list 'load-path
             (expand-file-name "cider" vendor-directory))

(require 'cider)


(setq nrepl-log-messages t)
(setq cider-show-error-buffer nil)
(setq cider-auto-select-error-buffer nil)


