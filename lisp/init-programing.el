
(setq auto-mode-alist
      (append
       '(
	 ("\\.js\\'" . js2-mode)
	 ("\\.html\\'" . web-mode)
	 ("\\.cl\\'" . lisp-mode)
	 )
       auto-mode-alist))

(require 'js2-refactor)
(add-hook 'js2-mode #'js2-refactor-mode)

(setq inferior-lisp-program "/usr/local/bin/sbcl")
(setq slime-contribs '(slime-fancy))

(provide 'init-programing)
