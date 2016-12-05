(defvar dotspacemacs-distribution 'spacemacs)

(defvar configuration-layer--declared-layers-usedp nil)

(defvar configuration-layer--package-properties-read-onlyp nil
  "If non-nil then package propertise are read only and cannot be overriden by
`configuration-layer/make-package'.")

(defvar configuration-layer--indexed-packages (make-hash-table :size 2048)
  "hash map to index `cfgl-package' objects by their name.")

(defconst configuration-layer-directory
  (expand-file-name (concat spacemacs-start-directory "layers/"))
  "Spacemacs contribution layers base directory.")

(defvar configuration-layer-categories '()
  "List of strings corresponding to category names. A category is a
directory with a name starting with `+'.")

(defvar configuration-layer--indexed-layers (make-hash-table :size 1024)
       "Hash map to index `cfgl-Layer' objects by their names.")

(defvar configuration-layer--used-layers '()
  "A non-sorted list of used layer names.")

(defvar configuration-layer--used-packages '()
  "An alphabetically sorted list of used package names.")

(defclass cfgl-layer ()
  ((name :initarg :name
	 :type symbol
	 :documentation "Name of the layer.")
   (dir :initarg :dir
	:initform nil
	:type (satisfies (lambda (x) (or (null x) (stringp x))))
	:documentation "Absolute path to the layer directory")
   (packages :initarg :packages
	     :initform nil
	     :type list
	     :documentation "List of package symbols declared in this layer")
   (selected-packages :initarg :selected-packages
		      :initform 'all
		      :type (satisfies (lambda (x) (or (and (symbolp x) (eq 'all x)) (listp x))))
		      :documentation "List of selected package symbols.")
   (variables :initarg :variables
	      :initform nil
	      :type list
	      :documentation "A list of variable-value pairs.")
   (lazy-install :initarg :lazy-install
		 :initform nil
		 :type boolean
		 :documentation
		 "If non-nil then the layer needs to be installed")
   (disabled :initarg :disabled-for
	     :initform nil
	     :type list
	     :documentation "A list of layers where this layer is disabled.")
   (enabled :initarg :enable-for
	    :initform 'unspecified
	    :type (satisfies (lambda (x) (or (listp x) (eq 'unspecified x))))
	    :documentation
	    ""))
  "A configuration layer")

(defmethod cfgl-layer-owned-packages ((layer cfgl-layer))
  "Return the list of owned packages by LAYER.
LAYER has to be installed for this method to work properly."

  )

(defmethod cfgl-layer-get-packages ((layer cfgl-layer))
  "Return the list of packages for LAYER."
  (if (eq 'all (oref layer :selected-packages))
      (oref layer :packages)
    (delq nil (mapcar
	       (lambda (x)
		 (let ((pkg-name (if (listp x) (car x) x)))
		   (when (memq pkg-name (oref layer :selected-packages)) x)))
	       (oref layer :packages)))))


(defclass cfgl-package ()
  ((name :initarg :name
	 :type symbol
	 :documentation "Name of the packages.")
   (min-version :initarg :min-version
		:initform nil
		:type list
		:documentation "Minimum version to install as a version list.")
   (owners :initarg :owners
	   :initform nil
	   :type list
	   :documentation "The layer defining the init function.")
   (pre-layers :initarg :pre-layers
	       :initform '()
	       :type list
	       :documentation "List of layers with a pre-init function")
   (post-layers :initarg :post-layers
		:initform '()
		:type list
		:documentation "List of layers with a post-init function")
   (location :initarg :location
	     :initform elpa
	     :type (satisfies (lambda (x)
				(or (stringp x)
				    (memq x '(built-in local site elpa))
				    (and (listp x) (eq 'recipe (car x))))))
	     :documentation "Location of the package")
   (toggle :initarg :toggle
	   :initform t
	   :type (satisfies (lambda (x) (or (symbolp x) (listp x))))
	   :documentation
	   "Package is enabled/installed if toggle evaluates to non-nil.")
   (step :initarg :step
	 :initform nil
	 :type (satisfies (lambda (x) (member x '(nil bootstrap pre))))
	 :documentation "Initialization step.")
   (lazy-install :initarg :lazy-install
		 :initform nil
		 :type boolean
		 :documentation
		 "If non-nil then the package needs to be installed")
   (protected :initarg :protected
	      :initform nil
	      :type boolean
	      :documentation
	      "If non-nil then the package cannot be excluded")
   (excluded :initarg :excluded
	     :initform nil
	     :type boolean
	     :documentation
	     "If non-nil this package is excluded from all layers.")))


(defmethod cfgl-package-enabledp ((pkg cfgl-package) &optional inhibit-message)
  "Evaluate the `toggle' slot fo passed PKG."
  (oref pkg :toggle))

(defmethod cfgl-package-set-property ((pkg cfgl-package) slot value)
  "Set SLOT to the given VALUE for the package PKG.
if `configuration-layer--package-properties-read-onlyp' is non-nil then VALUE
is not set for the given SLOT."
  (unless configuration-layer--package-properties-read-onlyp
    (eval `(oset pkg ,slot value)))
  )

(provide 'core-cfgl) 

