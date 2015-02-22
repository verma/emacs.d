;;; color-theme-adventure-time.el --- Cobalt Color Theme for Emacs
;;
;; Author: Nick Ewing
;; URL: https://github.com/nickewing/color-theme-adventure-time
;; Version: 0.0.2
;; Package-Requires: ((color-theme "6.6.1"))
;;
;; background changed by: Uday Verma (github.com/verma)

;; Defines a colour scheme resembling that of the original TextMate Cobalt color theme.
;; To use add the following to your .emacs file (requires the color-theme package):
;;
;; (require 'color-theme)
;; (color-theme-initialize)
;; (load-file "~/.emacs.d/themes/color-theme-adventure-time.el")
;;
;; And then (color-theme-adventure-time) to activate it.
;; Created using Marcus Crafter's Twilight Theme for Emacs as a template.
;; http://github.com/crafterm/twilight-emacs/blob/master/color-theme-twilight.el

;; Copyright (c) 2010 Nick Ewing <nick@nickewing.net>

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;;;###autoload
(defun color-theme-adventure-time ()
  "Color theme by Nick Ewing, based off the TextMate Cobalt theme, created 2010-04-16"
  (interactive)
  (color-theme-install
   '(color-theme-adventure-time
     ((background-color . "#24254D")
      (background-mode . dark)
      (border-color . "#24254D")
      (cursor-color . "#eeeeee")
      (foreground-color . "#F8F8F8"))
     (default ((t (:background "#24254D" :foreground "white"))))
     (buffers-tab ((t (:background "#24254D" :foreground "white"))))
     (font-lock-builtin-face ((t (:foreground "#40FFB9"))))
     (font-lock-comment-face ((t (:foreground "#008AFF"))))
     (font-lock-constant-face ((t (:foreground "#FF518C"))))
     (font-lock-doc-face ((t (:foreground "#008AFF"))))
     (font-lock-function-name-face ((t (:foreground "#FFDD00"))))
     (font-lock-keyword-face ((t (:foreground "#FA9E18"))))
     (font-lock-preprocessor-face ((t (:foreground "#8090A2"))))
     (font-lock-reference-face ((t (:foreground "#CCCCCC"))))
     (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "red"))))
     (font-lock-string-face ((t (:foreground "#84C44A"))))
     (font-lock-type-face ((t (:foreground "#FFEF79"))))
     (font-lock-variable-name-face ((t (:foreground "#CCCCCC"))))
     (font-lock-warning-face ((t (:foreground "Pink"))))
     (hl-line ((t (:background "#212446"))))
     (fringe ((t (:background "#24254D"))))
     (linum ((t (:background "#24254D" :foreground "#888888"
                 :underline nil))))
     (gui-element ((t (:background "#303030" :foreground "black"))))
     (region ((t (:background "#636BCB"))))
     (highlight ((t (:background "#404685"))))
     (show-paren-match ((t (:background "#26425D"))))
     (show-paren-mismatch ((t (:background "#FF0000"))))
     (ecb-default-highlight-face ((t (:background "#26425D"))))
     (minibuffer-prompt ((t (:foreground "#008AFF"))))
     (modeline ((t (:background "#111111" :foreground "#888888"))))
     (modeline-inactive ((t (:background "#222222" :foreground "#888888"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (toolbar ((t (nil))))
     (ido-subdir ((t (:foreground "#008AFF"))))
     (ido-only-match ((t (:foreground "#42D915"))))
     (mumamo-background-chunk-major ((t (:background nil))))
     (mumamo-background-chunk-submode1 ((t (:background nil))))
     (underline ((nil (:underline nil)))))))

(provide 'color-theme-adventure-time)

;;; color-theme-adventure-time.el ends here
