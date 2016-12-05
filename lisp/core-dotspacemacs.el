;;(defconst default-init (expand-file-name ".spacemacs" user-home-directory))
(defconst default-init (expand-file-name ".spacemacs" spacemacs-start-directory))
(defconst dotspacemacs-directory (concat user-home-directory ".spacemacs.d/"))

(defvar dotspacemacs-configuration-layers '(emacs-lisp)
  "List of configuration layers to load.")

(defvar dotspacemacs-mode-line-unicode-symbols t
  "If non nil unicode symbols are displayed in the mode-line (eg. for lighters)")

(defvar dotspacemacs-smartparens-strict-mode nil)

(defun dotspacemacs/load-file ()
  "Load ~/.spacemacs or .lmzemacs.d/init.el if it exists"
  (let ((dotspacemacs (dotspacemacs/location)))
	(if (file-exists-p dotspacemacs)
	    (with-demoted-errors "Error loading .spacemacs: %S" (load dotspacemacs))
	  (message "dotspacemacs not exist!"))
	))


(defmacro dotspacemacs|symbol-value (symbol)
  "Return the value of SYMBOL corresponding to a dotspacemacs variable.
If SYMBOL value is `display-graphic-p' then return the result of
`(display-graphic-p)', otherwise return the value of the symbol."
  `(if (eq 'display-graphic-p ,symbol) (display-graphic-p) ,symbol))

(defun dotspacemacs/location ()
  (let ((spacemacs-dir-init (when dotspacemacs-directory
			      (concat dotspacemacs-directory "init.el"))))
    (cond
     ((file-exists-p default-init) default-init)
     (t default-init))
    )
  )

(defmacro dotspacemacs|call-func (func &optional msg)
  `(progn
     (,func))
  )



(provide 'core-dotspacemacs)
