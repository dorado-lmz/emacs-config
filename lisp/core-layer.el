(defun configuration-layer/declare-layers (layers-specs)
  "Declare layers with LAYERS-SPECS."
  (mapc 'configuration-layer/declare-layer layers-specs))

(defun configuration-layer/declare-layer (layer-specs)
  "Declare a single layer with spec."
  (let* ((layer-name (if (listp layer-specs) (car layer-specs) layer-specs))
	 (layer (configuration-layer/get-layer layer-name))
	 (usedp configuration-layer--declared-layers-usedp))
    (if layer
	(let ((obj (configuration-layer/make-layer
		    layer-specs
		    (configuration-layer/get-layer layer-name)
		    usedp)))
	  (configuration-layer//add-layer obj usedp)
	  (configuration-layer//set-layer-variables obj)
	  (when usedp
	    (configuration-layer//load-layer-files layer-name '("layers.el"))))))
  )


(defun configuration-layer/get-layer (layer-name)
  "Return a layer object with name LAYER-NAME."
  (when (ht-contains? configuration-layer--indexed-layers layer-name)
    (ht-get configuration-layer--indexed-layers layer-name)))

(defun configuration-layer//add-layer (layer &optional usedp)
  "Add a LAYER object to the system.
USEDP non-nil means that PKG is a usedp layer."
  (let ((layer-name (oref layer :name)))
    (message (concat (if usedp "[used]! " "[indexed]! ") "layer ** %s **") layer-name)
    (puthash layer-name layer configuration-layer--indexed-layers)
    ;; add to used-layers list
    (when usedp
      (add-to-list 'configuration-layer--used-layers layer-name))))

(defun configuration-layer//set-layer-variables (layer)
  "Set the configuration variables for the passed LAYER."
  (let ((variables (oref layer :variables)))
    (while variables
      (let ((var (pop variables)))
	(if (consp variables)
	    (condition-case-unless-debug err
		(set-default var (eval (pop variables)))
	      ('error
	       (message "error")))))))
  )

(defun configuration-layer//load-layer-files (layer-name files)
  "Load the files of list FILES for the layer with the given LAYER-NAME"
  (let ((obj (configuration-layer/get-layer layer-name)))
    (when obj
      (dolist (file files)
	(let ((file (concat (oref obj :dir) file)))
	  (if (file-exists-p file) (load file)))))))


(defun configuration-layer/make-layer (layer-specs &optional obj usedp dir)
  "Return a `cfgl-layer' object based on LAYER-SPECS.
IF LOAD-PKGS is non-nil then load the `packages.el' file of the layer.
DIR is the directory where the layer is, if it is nil then search in the
indexed layers for the path"
  (let* ((layer-name (if (listp layer-specs) (car layer-specs) layer-specs))
	 (obj (if obj obj (cfgl-layer (symbol-name layer-name)
				      :name layer-name)))
	 (dir (or dir (oref obj :dir))))
    (if (or (null dir)
	    (and dir (not (file-exists-p dir))))
	(message "cannot make layer")
	(let* ((dir (file-name-as-directory dir))
	       (disabled (when (listp layer-specs)
			   (spacemacs/mplist-get layer-specs :disabled-for)))
	       (variables (when (listp layer-specs)
			    (spacemacs/mplist-get layer-specs :variables)))
	       (packages-file (concat dir "packages.el"))
	       (packages (if (file-exists-p packages-file)
			     (progn
			       (load packages-file)
			       (symbol-value (intern (format "%S-packages" layer-name))))
			    (oref obj :packages)))
	       (selected-packages (if packages
				      ;;(configuration-layer//select-packages
				      ;; layer-specs packages)
				      'all
				    ;;default value
				    'all)))
	  (oset obj :dir dir)
	  (when packages
	    (oset obj :packages packages)
	    (oset obj :selected-packages selected-packages))
	  obj))))

(defun configuration-layer/layer-usedp (layer-name)
  "Return non-nil if LAYER-NAME is the name of a used layer."
  (let ((obj (configuration-layer/get-layer layer-name)))
    (when obj (memq layer-name configuration-layer--used-layers))))
(provide 'core-layer)
