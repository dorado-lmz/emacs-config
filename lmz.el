(require 'cask "/usr/local/Cellar/cask/0.8.1/cask.el")
(cask-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(load-file (concat (file-name-directory load-file-name)
                   "lisp/core-load-paths.el"))

(require 'init-package)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-org)
(require 'init-programing)
(require 'init-outline)
(require 'init-keybinding)

(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))

(require 'core-spacemacs)
(spacemacs/init)

;;(configuration-layer/sync)
;;(spacemacs/setup-startup-hook)
