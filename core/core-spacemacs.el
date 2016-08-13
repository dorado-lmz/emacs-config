

(defconst emacs-start-time (current-time))

(require 'core-spacemacs-buffer)
(require 'core-dotspacemacs)
(require 'core-configuration-layer)
(require 'core-fonts-support)
(require 'core-funcs)

(defun spacemacs/init ()
  ;; explicitly set the prefered coding systems to avoid annoying prompt
  ;; from emacs (especially on Microsoft Windows)
  (prefer-coding-system 'utf-8)

  ;;load .spacemacs or .lmzemacs.d/init.el
  (dotspacemacs/load-file)
  ;; (require 'core-configuration-layer)
  (dotspacemacs|call-func dotspacemacs/init "Calling dotfile init...")
  (dotspacemacs|call-func dotspacemacs/user-init "Calling dotfile user init...")

  (configuration-layer/initialize)
  ;; font
  ;;(spacemacs|do-after-display-system-init
   ;; If you are thinking to remove this call to `message', think twice. You'll
   ;; break the life of several Spacemacser using Emacs in daemon mode. Without
   ;; this, their chosen font will not be set on the *first* instance of
   ;; emacsclient, at least if different than their system font. You don't
   ;; believe me? Go ahead, try it. After you'll have notice that this was true,
   ;; increase the counter bellow so next people will give it more confidence.
   ;; Counter = 1
   (message "Setting the font...")
   (unless (spacemacs/set-default-font dotspacemacs-default-font)
     (spacemacs-buffer/warning
      "Cannot find any of the specified fonts (%s)! Font settings may not be correct."
      (if (listp (car dotspacemacs-default-font))
          (mapconcat 'car dotspacemacs-default-font ", ")
        (car dotspacemacs-default-font)))))


(provide 'core-spacemacs)
