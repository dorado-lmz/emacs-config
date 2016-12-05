(require 'cl-lib)
(require 'eieio)
(require 'ht)
(require 'util-log)
(require 'core-keybindings) 
(require 'core-cfgl)
(require 'core-layer)
(require 'core-packages)

(defun configuration-layer/initialize ()
  "Initialize `package.el'."

  )

(defun configuration-layer/sync (&optional no-install)
  (interactive "P")
  ;; call function `dotspacemacs/layers' in dotfile then initialize
  ;; `dotspacemacs-configuration-layers'
  (dotspacemacs|call-func dotspacemacs/layers "Calling dotfile layers...")
  (setq dotspacemacs--configuration-layers-saved
	dotspacemacs-configuration-layers)


  (configuration-layer/discover-layers)
  (configuration-layer//declare--used-layers dotspacemacs-configuration-layers)
  (configuration-layer//declare--used-packages configuration-layer--used-layers)

  (configuration-layer//load-layers-files configuration-layer--used-layers
					  '("funcs.el"))
  (configuration-layer//configure-layers configuration-layer--used-layers)

  (configuration-layer//configure-packages configuration-layer--used-packages)
  (configuration-layer//load-layers-files configuration-layer--used-layers
					  '("keybindings.el"))

  )


(defun configuration-layer/discover-layers ()
  "Initialize `configuration-layer--indexed-layers' with layer directories."
  (setq configuration-layer--indexed-layers (make-hash-table :size 1024))
  (let ((search-paths (append (list configuration-layer-directory)
			      (when dotspacemacs-directory
				(list dotspacemacs-directory))))
	(discovered '()))
    (while search-paths
      (let ((current-path (car search-paths)))
	(setq search-paths (cdr search-paths))
	(dolist (sub (directory-files current-path t nil 'nosort))
	  ;; ignore ".", ".." and non-directories
	  (unless (or (string-equal ".." (substring sub -2))
		      (string-equal "." (substring sub -1))
		      (not (file-directory-p sub)))
	    (let ((type (configuration-layer//directory-type sub)))
	      (cond
	       ((eq 'category type)
		(let ((category (configuration-layer//get-category-from-path
				 sub)))
		  (spacemacs-buffer/message "-> Discovered category: %S"
					    category)
		  (push category configuration-layer-categories)
		  (setq search-paths (cons sub search-paths))))
	       ((eq 'layer type)
		(let* ((layer-name-str (file-name-nondirectory sub))
		       (layer-name (intern layer-name-str))
		       (indexed-layer (configuration-layer/get-layer
				       layer-name)))
		  (if indexed-layer
		      (unless (string-equal (oref indexed-layer :dir) sub)
			(oset indexed-layer :dir sub))
		    (configuration-layer//add-layer
			 (configuration-layer/make-layer layer-name nil nil sub)))))
	       (t
		(setq search-paths (cons sub search-paths))))))))))
  )


(defun configuration-layer//declare--used-layers (&optional layers-specs)
  (setq configuration-layer--used-layers nil)
  (let ((configuration-layer--declared-layers-usedp t))
    (dolist (layer-specs layers-specs)
      (let* ((layer-name (if (listp layer-specs) (car layer-specs) layer-specs))
	   (layer (configuration-layer/get-layer layer-name)))

      (if layer
	  (let ((layer-path (oref layer :dir)))
	    (unless (string-match-p "+distributions" layer-path)
	      (configuration-layer/declare-layer layer-specs)))
	(spacemacs-buffer/warning
	 "Unknown layer %s declared in dotfile!" layer-name))))
    (setq configuration-layer--used-layers
	  (reverse configuration-layer--used-layers))

    ;;distribution and bootstrap layers are always first
    (let ((distribution dotspacemacs-distribution))
      (unless (eq 'spacemacs-bootstrap distribution)
	(configuration-layer/declare-layer distribution)))
    (configuration-layer/declare-layer 'spacemacs-bootstrap)))


(defun configuration-layer//declare--used-packages (layers)
  "Declare used packages contained in LAYERS."
  (setq configuration-layer--used-packages nil)
  (let* ((warning-minimum-level :error))
    (configuration-layer/make-packages-from-layers layers t)
;;    (configuration-layer/make-packages-from-dotfile t)
    (setq configuration-layer--used-packages
	  (configuration-layer//sort-packages
	   configuration-layer--used-packages))))

(defun configuration-layer//load-layers-files (layer-names files)
  "Load the fiels of list FILES for all passed LAYER_NAMES."
  (dolist (layer-name layer-names)
    (configuration-layer//load-layer-files layer-name files)))

(defun configuration-layer//configure-layers (layer-names)
  "Configure layers with LAYER-NAMES."
  (let ((warning-minimum-level :error))
    (dolist (layer-name layer-names)
      (configuration-layer//load-layer-files layer-name '("config.el")))))

(defun configuration-layer//configure-packages (packages)
  "Configure all passed PACKAGES honoring the steps order."
  (spacemacs-buffer/message "+ Configuring bootstrap package....")
  (configuration-layer//configure-packages-2
   (configuration-layer//filter-objects
    packages (lambda (x)
	       (let ((pkg (configuration-layer/get-package x)))
		 (eq 'bootstrap (oref pkg :step))))))

  (spacemacs-buffer/message "+ Configuring pre package....")
  (configuration-layer//configure-packages-2
   (configuration-layer//filter-objects
    packages (lambda (x)
	       (let ((pkg (configuration-layer/get-package x)))
		 (eq 'pre (oref pkg :step))))))

  (spacemacs-buffer/message "+ Configuring package....")
  (configuration-layer//configure-packages-2
   (configuration-layer//filter-objects
    packages (lambda (x)
	       (let ((pkg (configuration-layer/get-package x)))
		 (null (oref pkg :step))))))
  )

(defun configuration-layer//configure-packages-2 (packages)
  "configure all passed PACKAGES."
  (dolist (pkg-name packages)
    (let ((pkg (configuration-layer/get-package pkg-name)))
      (cond
       ((oref pkg :lazy-install) 
	(spacemacs-buffer/message
	 (format "%S ignored since it can be lazily installed." pkg-name)))
       ((and (oref pkg :excluded)
	     (not (oref pkg :protected)))
	(spacemacs-buffer/message
	 (format "%S ignored since it has been excluded." pkg-name)))
       ((null (oref pkg :owners))
	(spacemacs-buffer/message
	 (format "%S ignored since it has no owner layer." pkg-name)))
       ((not (cfgl-package-enabledp pkg t))
	(spacemacs-buffer/message (format "%S is toggled off." pkg-name)))
       (t
	;;load-path
	(let ((location (oref pkg :location)))
	  (cond
	   ((stringp location)
	    (if (file-directory-p location)
		(push (file-name-as-directory location) load-path)
	      (configuration-layer//warning
	       "Location path for package %S does not exits (value: %s)."
	       pkg location)))
	   ((and (eq 'local location)
		 (eq 'dotfile (car (oref pkg :owners))))
	    (push (file-name-as-directory (concat configuration-layer-private-directory "local/"
						  (symbol-name (oref pkg :name))))
		  load-path))
	   ((eq 'local location)
	    (let* ((owner (configuration-layer/get-layer
			   (car (oref pkg :owners))))
		   (dir (when owner (oref owner :dir))))
	      (push (format "%slocal/%S/" dir pkg-name) load-path)))))
	;;configuration
	(unless (memq (oref pkg :location) '(local site built-in))
	  (message "asdf"))
	(cond
	 ((eq 'dotfile (car (oref pkg :owners)))
	  (spacemacs-buffer/message
	   (format "%S is configured in the dotfile." pkg-name)))
	 (t
	  (configuration-layer//configure-package pkg))))))))

(defun configuration-layer//configure-package (pkg)
  "Configure PKG object."
  (let* ((pkg-name (oref pkg :name))
	 (owner (car (oref pkg :owners))))
    
    ;;pre-init
    (mapc
     (lambda (layer)
       (when (configuration-layer/layer-usedp layer)
	 (if (not (configuration-layer//package-enabled-p pkg layer))
	     (spacemacs-buffer/message
	      (format " -> ignored pre-init (%S)" layer))
	   (spacemacs-buffer/message
	    (format " -> pre-init (%S)" layer))
	   (condition-case-unless-debug error
	       (funcall (intern (format "%S/pre-init-%S" layer pkg-name)))
	     ('error
	      )))))
     (oref pkg :pre-layers))
    
    ;;init
    (funcall (intern (format "%S/init-%S" owner pkg-name)))

    ;;post-init
    (mapc
     (lambda (layer)
       (when (configuration-layer/layer-usedp layer)
	 (if (not (configuration-layer//package-enabled-p pkg layer))
	     (spacemacs-buffer/message
	      (format "-> ignore post-init (%S)..." layer))
	   (spacemacs-buffer/message
	    (format "-> post-init (%S)..." layer))
	   (condition-case-unless-debug err
	       (funcall (intern (format "%S/post-init-%S" layer pkg-name)))
	     ('error)))))
     (oref pkg :post-layers))))

(defun configuration-layer//directory-type (path)
  "Return the type of directory pointed by PATH.
Possible return values:
  layer    - the directory is a layer
  category - the directory is a category
  nil      - the directory is a regular directory."
  (when (file-directory-p path)
    (if (string-match
         "^+" (file-name-nondirectory
               (directory-file-name
                (concat configuration-layer-directory path))))
        'category
      (let ((files (directory-files path)))
        ;; most frequent files encoutered in a layer are tested first
        (when (or (member "packages.el" files)
                  (member "layers.el" files)
                  (member "config.el" files)
                  (member "keybindings.el" files)
                  (member "funcs.el" files))
          'layer)))))

(defun configuration-layer//filter-objects (objects ffunc)
  "Return a filtered OBJECTS list where each element satisfies FFUNC."
  (reverse (cl-reduce (lambda (acc x) (if (funcall ffunc x) (push x acc) acc))
		      objects
		      :initial-value nil)))

(defun configuration-layer//get-category-from-path (dirpath)
  "Return a category symbol from the given DIRPATH.
The directory name must start with `+'.
Returns nil if the directory is not a category."

  (when (file-directory-p dirpath)
    (let ((dirname (file-name-nondirectory
		    (directory-file-name
		     (concat configuration-layer-directory
			     dirpath)))))
      (when (string-match "^+" dirname)
	(intern (substring dirname 1))))))
(provide 'core-configuration-layer)
