(defun configuration-layer/make-packages-from-layers (layer-names &optional usedp)
  "Read the package lists of layers with name LAYER_NAMES and create packages.
USEDP if non-nil indicates that made packages are used packages.
DOTFILE if non-file will process the dotfile `dotspacemacs-additional-packages'
variable as well"
  (dolist (layer-name layer-names)
    (let ((layer (configuration-layer/get-layer layer-name)))
      (dolist (pkg (cfgl-layer-get-packages layer))
	(let* ((pkg-name (if (listp pkg) (car pkg) pkg))
	       (obj (configuration-layer/get-package pkg-name)))
	  (setq obj (configuration-layer/make-package pkg layer-name obj))
	  (configuration-layer//add-package
	   obj usedp))))))

(defun configuration-layer/make-package (pkg layer-name &optional obj)
  "Return a `cfgl-package' objects based on PKG.
If OBJ is non nil then copy PKG properties into OBJ, otherwise create
a new object.
Properties that can be copied are `:location', `:step' and `:excluded'.
If TOGGLEP is nil then `:toggle' parameter is ignored."
  (let* ((pkg-name (if (listp pkg) (car pkg) pkg))
	 (pkg-name-str (symbol-name pkg-name))
	 (layer (unless (eq 'dotfile layer-name)
		  (configuration-layer/get-layer layer-name)))
	 (min-version (when (listp pkg) (plist-get (cdr pkg) :min-version)))
	 (step (when (listp pkg) (plist-get (cdr pkg) :step)))
	 (toggle (when (listp pkg) (plist-get (cdr pkg) :toggle)))
	 (excluded (when (listp pkg) (plist-get (cdr pkg) :excluded)))
	 (location (when (listp pkg) (plist-get (cdr pkg) :location)))
	 (protected (when (listp pkg) (plist-get (cdr pkg) :protected)))
	 (init-func (intern (format "%S/init-%S"
				    layer-name pkg-name)))
	 (pre-init-func (intern (format "%S/pre-init-%S"
					layer-name pkg-name)))
	 (post-init-func (intern (format "%S/post-init-%S"
					 layer-name pkg-name)))
	 (copyp (not (null obj)))
	 (obj (if obj obj (cfgl-package pkg-name-str :name pkg-name)))
	 (ownerp (or (and (eq 'dotfile layer-name)
			  (null (oref obj :owners)))
		     (fboundp init-func))))
    (when min-version
      (cfgl-package-set-property obj :min-version (version-to-list min-version)))
    (when step (cfgl-package-set-property obj :step step))
    (when toggle (cfgl-package-set-property obj :toggle toggle))
    (cfgl-package-set-property obj :excluded
			       (and (configuration-layer/layer-usedp layer-name)
				    (or excluded (oref obj :excluded))))
    (when location
      (if (and (listp location)
	       (eq (car location) 'recipe)
	       (eq (plist-get (cdr location) :fetcher) 'local))
	  (cond
	   (layer (let ((path (expand-file-name
			       (format "%s%s/%s.el"
				       (configuration-layer/get-layer-local-dir
					layer-name)
				       pkg-name-str pkg-name-str))))
		    (cfgl-package-set-property
		     obj :location `(recipe :fetcher file :path ,path))))
	   ((eq 'dotfile layer-name)
	    nil))
	(cfgl-package-set-property obj :location location)))

    (unless copyp
      ;;a bootstrap pacakge is protected
      (cfgl-package-set-property
       obj :protected (or protected (eq 'bootstrap step)))
      (when protected
	(push pkg-name configuration-layer--protected-packages)))

    (when ownerp
      ;;warn about multiple owners
      (when (and (oref obj :owners)
		 (not (memq layer-name (oref obj :owners))))
	(spacemacs-buffer/warning
	 (format (concat "More than one init function found for "
			 "package %S. Previous owner was %S, "
			 "replacing it with layer %S.")
		 pkg-name (car (oref obj :owners)) layer-name)))

      (object-add-to-list obj :owners layer-name))

    (unless (or ownerp
		(eq 'dotfile layer-name)
		(fboundp pre-init-func)
		(fboundp post-init-func)
		(oref obj :excluded))
      (spacemacs-buffer/warning
       (format (concat "package %s not initialized in layer %s, "
		       "you may consider removing this package from "
		       "the package list or use the :toggle keyword "
		       "instead of a `when' form.")
	       pkg-name layer-name)))

    (when (and (not ownerp)
	       (and (not (eq 'unspecified toggle))
		    toggle))
      (spacemacs-buffer/warning
       (format (concat "Ignoring :toggle for package %s because"
		       "layer %s does not own it.")
	       pkg-name layer-name)))

    (when (fboundp pre-init-func)
      (object-add-to-list obj :pre-layers layer-name))

    (when (fboundp post-init-func)
      (object-add-to-list obj :post-layers layer-name))
    obj))

(defun configuration-layer//package-enable-p (pkg layer)
  "Return ture if PKG should be configured for LAYER.
LAYER must not be the owner of PKG."
  (let* ((owner (configuration-layer/get-layer (car (oref pkg :owners))))
	 (disabled (oref pkg :disabled-for))
	 (enabled (oref pkg :enabled-for)))
    (if (not (eq 'unspecified enabled))
	(memq layer enabled)
      (not (memq layer diabled)))))

(defun configuration-layer/get-package (pkg-name)
  "Return a package object with name PKG-NAME.
return nil if package object is not found."
  (when (ht-contains? configuration-layer--indexed-packages pkg-name)
    (ht-get configuration-layer--indexed-packages pkg-name)))

(defun configuration-layer/package-usedp (name)
  "Return non-nil if NAME is the name of a used package."
  (let ((obj (configuration-layer/get-package name)))
        (not (null obj))))

(defun configuration-layer/make-package-from-dotfile (&optional usedp)
  "Read the addtional packages declared in the dotfile and create packages.
USEDP id non-nil indicates that made packages are used packages."
  (dolist (pkg dotspacemacs-additional-packages)
    (let* ((pkg-name (if (listp pkg) (car pkg) pkg))
	   (obj (configuration-layer/get-package pkg-name)))
      (if obj
	  (setq obj (configuration-layer/make-package pkg 'dotfile obj))
	(setq obj (configuration-layer/make-package pkg 'dotfile)))

      (configuration-layer//add-package obj usedp)
      ))
  )

(defun configuration-layer//add-package (pkg &optional usedp)
  "Add a PKG object to the system.
USEDP non-nil means that PKG is a usedp package."
  (let ((pkg-name (oref pkg :name)))
	(puthash pkg-name pkg configuration-layer--indexed-packages)
	(when usedp
	  (add-to-list 'configuration-layer--used-packages pkg-name))))

(defun configuration-layer//sort-packages (packages)
  "Return a sorted list of PACKAGES objects"
  (sort packages (lambda (x y) (string< (symbol-name x) (symbol-name y)))))

(provide 'core-packages)
