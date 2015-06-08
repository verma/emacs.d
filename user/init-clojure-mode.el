;; initialize clojure mode
;;
(provide 'init-clojure-mode)


(require 'clojure-mode)

;; we want paredit when editing clojure, along with evil bindings
;;
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'evil-paredit-mode)

;; Prettify certain clojure things
;;
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

(dolist (v om-methods)
  (put-clojure-indent v 'defun))

