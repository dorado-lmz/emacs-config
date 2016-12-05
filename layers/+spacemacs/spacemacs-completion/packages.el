(setq spacemacs-completion-packages
      '(
	(default-ivy-config :location built-in)
	))

(defun spacemacs-completion/init-default-ivy-config ()
  (with-eval-after-load 'ivy
    (setq ivy-height 15
	  ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
    
    ))
