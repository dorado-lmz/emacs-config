(defvar spacemacs-start-directory
  user-emacs-directory
  "Spacemacs start directory.")


(require 'core-dotspacemacs)
(require 'core-configuration-layer)
(require 'util-funs) 
(require 'util-toggle)
(require 'core-fonts-support)
(defun spacemacs/init ()

  ;; load .spacemacs or .lmzemacs.d/init.el
  (dotspacemacs/load-file)

  (configuration-layer/sync)
  )

(provide 'core-spacemacs)
