(defconst spacemacs-bootstrap-packages '(
			 (bind-map :step bootstrap)
			 (diminish :step bootstrap)
			 (evil :step bootstrap)
			 (which-key :step bootstrap)
			 outshine
			 reveal-in-osx-finder
			 helm-ag
			 ))

(defun spacemacs-bootstrap/init-bind-map ()
  (require 'bind-map)
  (bind-map spacemacs-default-map
    :keys ("M-m")
    :evil-keys ("<SPC>")))

(defun spacemacs-bootstrap/init-diminish ()
  (when (not (configuration-layer/package-usedp 'spaceline))
    (add-hook 'after-load-functions 'spacemacs/diminish-hook)))

(defun spacemacs-bootstrap/init-evil ()
  (evil-mode 1)
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state))

(defun spacemacs-bootstrap/init-outshine()
  (use-package outshine
    :init))

(defun spacemacs-bootstrap/init-reveal-in-osx-finder()
  (use-package reveal-in-osx-finder
    :init))

(defun spacemacs-bootstrap/init-helm-ag()
  (use-package helm-ag
    :init))

(defun spacemacs-bootstrap/init-which-key ()
  (which-key-mode t)
  (setq which-key-side-window-location 'bottom)
  )
