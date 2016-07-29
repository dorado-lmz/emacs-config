
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;(require 'org-install)
;;(require 'ob-tangle)
;;(org-babel-load-file (expand-file-name "lmz.org" user-emacs-directory))

(package-initialize)

(require 'cask "/usr/local/Cellar/cask/0.7.4/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode t)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-package)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-keybinding)
(require 'init-org)

(defun open-my-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(require 'popwin)
(popwin-mode t)

(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
(put 'dired-find-alternate-file 'disabled nil)


