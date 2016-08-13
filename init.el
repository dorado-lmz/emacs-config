;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;;(require 'org-install)
;;(require 'ob-tangle)
;;(org-babel-load-file (expand-file-name "lmz.org" user-emacs-directory))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(setq debug-on-error t)
(defconst spacemacs-emacs-min-version   "24.4" "Minimal version of Emacs.")

(if (not (version<= spacemacs-emacs-min-version emacs-version))
    (message (concat "Your version of Emacs (%s) is too old. "
                     "Spacemacs requires Emacs version %s or above.")
             emacs-version spacemacs-emacs-min-version)
  (load-file (concat (file-name-directory load-file-name)
                     "core/core-load-paths.el"))

  (require 'core-spacemacs)
  (spacemacs/init)
  (configuration-layer/sync)

  (add-to-list 'load-path "~/.emacs.d/lisp/")

  (require 'init-package)
  (require 'init-ui)
  (require 'init-better-defaults)
  (require 'init-keybinding)
  (require 'init-org)
  (require 'init-programing)
  (require 'init-outline)

  (require 'popwin)
  (popwin-mode t)

  (setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
  (put 'dired-find-alternate-file 'disabled nil)
)
(put 'erase-buffer 'disabled nil)
