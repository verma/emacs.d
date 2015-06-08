;; initialize evil mode
;;
(provide 'init-evil)

(add-to-list 'load-path
             (expand-file-name "evil" vendor-directory))

(require 'evil)
(require 'evil-leader)
(require 'evil-paredit)

;; keep the evil mode turned on all the time
;;
(evil-mode t)

;; Setup evil leader
(global-evil-leader-mode)
(evil-leader/set-leader ",")

;; some wrap functionality we're used to
;;
(evil-leader/set-key
  "W" 'paredit-wrap-round
  "w[" 'paredit-wrap-square
  "w{" 'paredit-wrap-curly
  "S" 'paredit-splice-sexp
  "R" 'paredit-raise-sexp)
