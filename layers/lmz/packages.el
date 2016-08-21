(defconst lmz-packages '(
			 outshine
			 reveal-in-osx-finder
			 helm-ag
			 ))

(defun lmz/init-outshine()
  (use-package outshine
    :defer t
    :init))
(defun lmz/init-reveal-in-osx-finder()
  (use-package reveal-in-osx-finder
    :defer t
    :init))
(defun lmz/init-helm-ag()
  (use-package helm-ag
    :defer t
    :init))

