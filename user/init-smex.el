;; smex

(provide 'init-smex)

(require 'smex)

; initialize smex and bind keys
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
