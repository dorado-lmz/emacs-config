
(load-file (concat (file-name-directory load-file-name)
                   "core/core-load-paths.el"))

(require 'core-spacemacs)
(spacemacs/init)

(configuration-layer/sync)
(spacemacs/setup-startup-hook)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-package)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-org)
(require 'init-programing)
(require 'init-outline)
(require 'init-keybinding)

(require 'popwin)
(popwin-mode t)

(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
(put 'dired-find-alternate-file 'disabled nil)
