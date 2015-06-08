;; fiplr
;;
(provide 'init-fiplr)

(setq fiplr-root-markers '(".git" ".svn"))
(setq fiplr-ignored-globs '((directories (".git" ".svn" ".repl" "out" "target" "node_modules"))
                            (files ("*.jpg" "*.png" "*.zip" "*~" ".*" "#*"))))

;; make ctrl-t trigger flipr find file
(global-set-key (kbd "C-x C-p") 'fiplr-find-file)

(require 'fiplr)

