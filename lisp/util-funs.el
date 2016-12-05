(defun spacemacs/system-is-mac ()
  (eq system-type 'darwin))

(defun spacemacs/system-is-linux ()
  (eq system-type 'gnu/linux))

(defun spacemacs/system-is-mswindows ()
  (eq system-type 'window-nt))

(defun spacemacs/mplist-get (plist prop)
  "Get the values assosiated to PROP in PLIST, a modified plist.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the first property
and its value is returned.

Currently this function infloops when the list is circular."
  (let ((tail plist)
	result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;;pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(defun spacemacs//create-key-binding-form (props func)
  "Helper which returns a form to bind FUNC to a key according to PROPS.

Supported properties:

`:evil-leader STRING'
    One or several key sequence strings to be set wich `spacemacs/set-leader-keys'.

`:evil-leader-for-mode CONS CELL'
    One or several cons cells (MODE . KEY) where MODe is a major-mode symbol
    and KEY is a key sequence string to be set with
    `spacemacs/set-leader-keys-for-major-mode'.

`:global-key STRING'
    One or sereral key sequence strings to be set with `global-set-key'.

`:define-key CONS CELL'
    One or serveral cons cells (MAP . KEY) where MAP is a mode map and KEY is a
    KEY sequence string to be set with `define-key'."

  (let ((evil-leader (spacemacs/mplist-get props :evil-leader))
	(evil-leader-for-mode (spacemacs/mplist-get props :evil-leader-for-mode))
	(global-key (spacemacs/mplist-get props :global-key))
	(def-key (spacemacs/mplist-get props :define-key)))

    (append
     (when evil-leader
       `((dolist (key ',evil-leader)
	   (spacemacs/set-leader-keys key ',func))))
     (when evil-leader-for-mode
       `((dolist (val ',evil-leader-for-mode)
	   (spacemacs/set-leader-keys-for-major-mode
	    (car val) (cdr val) ',func))))
     (when global-key
       `((dolist (key ',global-key)
	   (global-set-key (kbd key) ',func))))
     (when def-key
       `((dolist (val ',def-key)
	   (define-key (eval (car val)) (kbd (cdr val)) ',func)))))))

(defun spacemacs/diminish-hook (_)
  "Display diminished lighter in vanilla Emacs mode-line."
  (let ((unicodep (dotspacemacs|symbol-value
		   dotspacemacs-mode-line-unicode-symbols)))
    (cl-loop for (mode uni nouni) in spacemacs--diminished-minor-modes
	     do (diminish mode (if unicodep uni nouni)))))

(defun spacemacs/add-to-hooks (fun hooks)
  "Add function to hooks"
  (dolist (hook hooks)
    (add-hook hook fun)))

(provide 'util-funs)

