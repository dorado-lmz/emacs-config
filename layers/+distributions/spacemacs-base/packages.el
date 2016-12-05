(setq spacemacs-base-packages
      '(
	(abbrev :location built-in)
	(bookmark :location built-in)
	(dired :location built-in)
	(dired-x :location built-in)
	evil-escape
	(exec-path-from-shell :step pre)
	(imenu :location built-in)
	(linum :location built-in)
	(recentf :location built-in)
	))

(defun spacemacs-base/init-abbrev ()
  )

(defun spacemacs-base/init-bookmark ()
  (use-package bookmark
    :init
    ))

(defun spacemacs-base/init-dired ()
  (spacemacs/set-leader-keys
   "ad" 'dired
   "fj" 'dired-jump))


(defun spacemacs-base/init-evil-escape ()
  (use-package evil-escape
    :init (evil-escape-mode)))

(defun spacemacs-base/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :init (when (or (spacemacs/system-is-mac)
		    (spacemacs/system-is-linux)
		    (memq window-system '(x)))
	    (exec-path-from-shell-initialize))))


(defun spacemacs-base/init-imenu ()
  (use-package imenu
    :defer t
    :init (spacemacs/set-leader-keys "ji" 'imenu)))

(defun spacemacs-base/init-linum ()
  (add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'text-mode-hook 'linum-mode)
  (spacemacs|add-toggle line-numbers
			:mode linum-mode
			:documentation "Show the line numbers."
			:evil-leader "tn"))
(defun spacemacs-base/init-recentf ()
  (use-package recentf
    :defer t
    :init
    (progn
      (add-hook 'find-file-hook (lambda () (unless recentf-mode
					     (recentf-mode)
					     (recentf-track-opened-file))))
      (setq recentf-max-saved-items 1000))
    
    :config
    (progn
      (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
      (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))))


