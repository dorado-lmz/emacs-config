(defvar spacemacs-insecure nil
    "If non-nil force Spacemacs to operate without secured protocols.")
(require 'cl-lib)
(require 'eieio)
(require 'package)
(require 'ht)

(defvar configuration-layer--inhibit-warnings nil
  "If non-nil then warning message emitted by the layer system are ignored.")

(defvar configuration-layer--load-packages-files nil
  "If non-nil force loading `packages.el' files when creating layer objects.")

(defvar configuration-layer--package-archives-refreshed nil
  "Non nil if package archives have already been refreshed.")

(defvar configuration-layer--delayed-layers '()
  "A list of layer names to check again after first pass of package loading.")

(defvar configuration-layer-exclude-all-layers nil
  "If non nil then only the distribution layer is loaded.")

(defvar configuration-layer--used-layers '()
  "A non-sorted list of used layer names.")

(defvar configuration-layer-force-distribution nil
  "If set, bypass the user's choice `dotspacemacs-distribution'.")

(defvar configuration-layer--used-distant-packages '()
  "A list of all distant packages that are effectively used.")

(defvar configuration-layer--used-packages '()
  "An alphabetically sorted list of used package names.")

(defvar configuration-layer--indexed-packages (make-hash-table :size 2048)
  "Hash map to index `cfgl-package' objects by their names.")

(defconst configuration-layer-directory
  (expand-file-name (concat spacemacs-start-directory "layers/"))
  "Spacemacs contribution layers base directory.")

(defvar configuration-layer-categories '()
  "List of strings corresponding to category names. A category is a
directory with a name starting with `+'.")

(defvar configuration-layer--indexed-layers (make-hash-table :size 1024)
  "Hash map to index `cfgl-layer' objects by their names.")

(defvar configuration-layer--protected-packages nil
  "A list of packages that will be protected from removal as orphans.")

(defvar configuration-layer--refresh-package-timeout dotspacemacs-elpa-timeout
  "Timeout in seconds to reach a package archive page.")

(defvar configuration-layer--elpa-archives
  '(("melpa" . "melpa.org/packages/")
    ("org"   . "orgmode.org/elpa/")
    ("gnu"   . "elpa.gnu.org/packages/"))
  "List of ELPA archives required by Spacemacs.")

(defun configuration-layer//install-quelpa ()
  "Install `quelpa'."
  (setq quelpa-verbose init-file-debug
        quelpa-dir (concat spacemacs-cache-directory "quelpa/")
        quelpa-build-dir (expand-file-name "build" quelpa-dir)
        quelpa-persistent-cache-file (expand-file-name "cache" quelpa-dir)
        quelpa-update-melpa-p nil)
  (configuration-layer/load-or-install-protected-package 'package-build)
  (configuration-layer/load-or-install-protected-package 'quelpa))

(defclass cfgl-package ()
  ((name :initarg :name
         :type symbol
         :documentation "Name of the package.")
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
               :documentation "List of layers with a pre-init function.")
   (post-layers :initarg :post-layers
               :initform '()
               :type list
               :documentation "List of layers with a post-init function.")
   (location :initarg :location
             :initform elpa
             :type (satisfies (lambda (x)
                                (or (stringp x)
                                    (memq x '(built-in local site elpa))
                                    (and (listp x) (eq 'recipe (car x))))))
             :documentation "Location of the package.")
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
              "If non-nil then this package cannot be excluded.")
   (excluded :initarg :excluded
             :initform nil
             :type boolean
             :documentation
             "If non-nil this package is excluded from all layers.")))

(defmethod cfgl-package-get-safe-owner ((pkg cfgl-package))
  "Safe method to return the name of the layer which owns PKG."
  ;; The owner of a package is the first *used* layer in `:owners' slot.
  ;; Note: for packages in `configuration-layer--used-packages' the owner is
  ;; always the car of the `:owners' slot.
  (let ((layers (oref pkg :owners)))
    (while (and (consp layers)
                (not (configuration-layer/layer-usedp (car layers))))
      (pop layers))
    (when (configuration-layer/layer-usedp (car layers))
      (car layers))))

(defmethod cfgl-package-enabledp ((pkg cfgl-package) &optional inhibit-messages)
  "Evaluate the `toggle' slot of passed PKG."
  (let ((message-log-max (unless inhibit-messages message-log-max))
        (toggle (oref pkg :toggle)))
    (eval toggle)))

(defclass cfgl-layer ()
  ((name :initarg :name
         :type symbol
         :documentation "Name of the layer.")
   (dir :initarg :dir
        :initform nil
        :type (satisfies (lambda (x) (or (null x) (stringp x))))
        :documentation "Absolute path to the layer directory.")
   (packages :initarg :packages
             :initform nil
             :type list
             :documentation "List of package symbols declared in this layer.")
   (selected-packages :initarg :selected-packages
             :initform 'all
             :type (satisfies (lambda (x) (or (and (symbolp x) (eq 'all x))
                                              (listp x))))
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
             :documentation "A list of layer where this layer is disabled."))
  "A configuration layer.")


(defmethod cfgl-layer-get-packages ((layer cfgl-layer))
  "Return the list of packages for LAYER."
  (if (eq 'all (oref layer :selected-packages))
      (oref layer :packages)
    (delq nil (mapcar
               (lambda (x)
                 (let ((pkg-name (if (listp x) (car x) x)))
                   (when (memq pkg-name (oref layer :selected-packages)) x)))
               (oref layer :packages)))))

(defun configuration-layer//add-package (pkg &optional usedp)
  "Add a PKG object to the system.
USEDP non-nil means that PKG is a used package."
  (let ((pkg-name (oref pkg :name)))
    (message "add package < %s >" pkg-name)
    (if usedp
	(message "index!")
      (message "used!"))
    (puthash pkg-name pkg configuration-layer--indexed-packages)
    (when usedp
      (add-to-list 'configuration-layer--used-packages pkg-name))))

(defun configuration-layer//add-layer (layer &optional usedp)
  "Add a LAYER object to the system.
USEDP non-nil means that PKG is a used layer."
  (let ((layer-name (oref layer :name)))
    (message "add layer ** %s **" layer-name)
    (puthash layer-name layer configuration-layer--indexed-layers)
    (when usedp
      (add-to-list 'configuration-layer--used-layers layer-name))))

(defun configuration-layer/make-packages (layer-names &optional usedp dotfile)
  "Read the package lists of layers with name LAYER-NAMES and create packages.
USEDP if non-nil indicates that made packages are used packages.
DOTFILE if non-nil will process the dotfile `dotspacemacs-additional-packages'
variable as well."
  (dolist (layer-name layer-names)
    (let ((layer (configuration-layer/get-layer layer-name)))
      (dolist (pkg (cfgl-layer-get-packages layer))
        (let* ((pkg-name (if (listp pkg) (car pkg) pkg))
               (init-func (intern (format "%S/init-%S"
                                          layer-name pkg-name)))
               (pre-init-func (intern (format "%S/pre-init-%S"
                                              layer-name pkg-name)))
               (post-init-func (intern (format "%S/post-init-%S"
                                               layer-name pkg-name)))
               (ownerp (fboundp init-func))
               (obj (configuration-layer/get-package pkg-name)))
          (if obj
              (setq obj (configuration-layer/make-package pkg obj ownerp layer))
            (setq obj (configuration-layer/make-package pkg nil ownerp layer)))
          (configuration-layer//add-package obj usedp)
          (when ownerp
            ;; last owner wins over the previous one,
            ;; still warn about mutliple owners
            (when (and (oref obj :owners)
                      (not (memq layer-name (oref obj :owners))))
              (configuration-layer//warning
               (format (concat "More than one init function found for "
                               "package %S. Previous owner was %S, "
                               "replacing it with layer %S.")
                       pkg-name (car (oref obj :owners)) layer-name)))
            (push layer-name (oref obj :owners)))
          ;; if no function at all is found for the package, then check
          ;; again this layer later to resolve `package-usedp'  usage in
          ;; `packages.el' files
          (unless (or ownerp
                      (fboundp pre-init-func)
                      (fboundp post-init-func)
                      (oref obj :excluded))
            (unless (memq layer-name configuration-layer--delayed-layers)
              (configuration-layer//warning
               (format (concat "package %s not initialized in layer %s, "
                               "you may consider removing this package from "
                               "the package list or use the :toggle keyword "
                               "instead of a `when' form.")
                       pkg-name layer-name))
              (push layer-name configuration-layer--delayed-layers)))
          ;; check if toggle can be applied
          (when (and (not ownerp)
                     (listp pkg)
                     (spacemacs/mplist-get pkg :toggle))
            (configuration-layer//warning
             (format (concat "Ignoring :toggle for package %s because "
                             "layer %S does not own it.")
                     pkg-name layer-name)))
          (when (fboundp pre-init-func)
            (push layer-name (oref obj :pre-layers)))
          (when (fboundp post-init-func)
            (push layer-name (oref obj :post-layers)))))))
  ;; additional and excluded packages from dotfile
 ;; (when dotfile
 ;;   (dolist (pkg dotspacemacs-additional-packages)
 ;;     (let* ((pkg-name (if (listp pkg) (car pkg) pkg))
 ;;            (obj (configuration-layer/get-package pkg-name)))
 ;;       (if obj
 ;;           (setq obj (configuration-layer/make-package pkg obj t))
 ;;         (setq obj (configuration-layer/make-package pkg nil t))
 ;;         (push 'dotfile (oref obj :owners)))
 ;;       (configuration-layer//add-package obj usedp)))
 ;;   (dolist (xpkg dotspacemacs-excluded-packages)
 ;;     (let ((obj (configuration-layer/get-package xpkg)))
 ;;       (unless obj
 ;;         (setq obj (configuration-layer/make-package xpkg)))
 ;;       (configuration-layer//add-package obj usedp)
 ;;       (oset obj :excluded t)))))
)

(defun configuration-layer/make-package (pkg &optional obj togglep layer)
  "Return a `cfgl-package' object based on PKG.
If OBJ is non nil then copy PKG properties into OBJ, otherwise create
a new object.
Properties that can be copied are `:location', `:step' and `:excluded'.
If TOGGLEP is nil then `:toggle' parameter is ignored."
  (let* ((name-sym (if (listp pkg) (car pkg) pkg))
         (name-str (symbol-name name-sym))
         (location (when (listp pkg) (plist-get (cdr pkg) :location)))
         (min-version (when (listp pkg) (plist-get (cdr pkg) :min-version)))
         (step (when (listp pkg) (plist-get (cdr pkg) :step)))
         (excluded (when (listp pkg) (plist-get (cdr pkg) :excluded)))
         (toggle (when (and togglep (listp pkg)) (plist-get (cdr pkg) :toggle)))
         (protected (when (listp pkg) (plist-get (cdr pkg) :protected)))
         (copyp (not (null obj)))
         (obj (if obj obj (cfgl-package name-str :name name-sym))))
    (when (and (listp location)
               (eq (car location) 'recipe)
               (eq (plist-get (cdr location) :fetcher) 'local))
      (setq location
            `(recipe
              :fetcher file
              :path ,(expand-file-name
                      (format
                       "%s%s/%s.el"
                       (configuration-layer/get-layer-local-dir
                        (oref layer :name))
                       name-str name-str)))))
    (when location (oset obj :location location))
    (when min-version (oset obj :min-version (version-to-list min-version)))
    (when step (oset obj :step step))
    (oset obj :excluded (or excluded (oref obj :excluded)))
    (when toggle (oset obj :toggle toggle))
    ;; cannot override protected packages
    (unless copyp
      ;; a bootstrap package is protected
      (oset obj :protected (or protected (eq 'bootstrap step)))
      (when protected
        (push name-sym configuration-layer--protected-packages)))
    obj))


(defun configuration-layer/make-layer (layer-specs &optional obj usedp dir)
  "Return a `cfgl-layer' object based on LAYER-SPECS.
If LOAD-PKGS is non-nil then load the `packages.el' file of the layer.
DIR is the directory where the layer is, if it is nil then search in the
indexed layers for the path."
  (let* ((layer-name (if (listp layer-specs) (car layer-specs) layer-specs))
         (obj (if obj obj (cfgl-layer (symbol-name layer-name)
                                      :name layer-name)))
         (dir (or dir (oref obj :dir))))
    (if (or (null dir)
            (and dir (not (file-exists-p dir))))
        (configuration-layer//warning
         "Cannot make layer %S without a valid directory!"
         layer-name)
      (let* ((dir (file-name-as-directory dir))
             (disabled (when (listp layer-specs)
                         (spacemacs/mplist-get layer-specs :disabled-for)))
             (variables (when (listp layer-specs)
                          (spacemacs/mplist-get layer-specs :variables)))
             (packages-file (concat dir "packages.el"))
             (packages
              (if (and (or usedp configuration-layer--load-packages-files)
                       (file-exists-p packages-file))
                  (progn
                    (load packages-file)
                    (symbol-value (intern (format "%S-packages" layer-name))))
                (oref obj :packages)))
             (selected-packages (if packages
                                    (configuration-layer//select-packages
                                     layer-specs packages)
                                  ;; default value
                                  'all)))
        (oset obj :dir dir)
        (when usedp
          (oset obj :disabled-for disabled)
          (oset obj :variables variables))
        (when packages
          (oset obj :packages packages)
          (oset obj :selected-packages selected-packages))
        obj))))


(defun configuration-layer/remove-layers (layer-names)
  "Remove layers with LAYER-NAMES from used layers."
  (mapc 'configuration-layer/remove-layer layer-names))

(defun configuration-layer/remove-layer (layer-name)
  "Remove an used layer with name LAYER-NAME."
  (setq configuration-layer--used-layers
        (delq layer-name configuration-layer--used-layers)))

(defun configuration-layer/get-layer (layer-name)
  "Return a layer object with name LAYER-NAME.
Return nil if layer object is not found."
  (when (ht-contains? configuration-layer--indexed-layers layer-name)
    (ht-get configuration-layer--indexed-layers layer-name)))

(defun configuration-layer/get-packages-list ()
  "Return a list of all package symbols."
  (ht-keys configuration-layer--indexed-packages))
(defun configuration-layer/get-package (pkg-name)
  "Return a package object with name PKG-NAME.
Return nil if package object is not found."
  (when (ht-contains? configuration-layer--indexed-packages pkg-name)
    (ht-get configuration-layer--indexed-packages pkg-name)))

(defun configuration-layer//get-distant-packages (packages usedp)
  "Return the distant packages (ie to be intalled).
If USEDP is non nil then returns only the used packages; if it is nil then
return both used and unused packages."
  (configuration-layer/filter-objects
   packages (lambda (x)
              (let ((pkg (configuration-layer/get-package x)))
                (and (not (memq (oref pkg :location) '(built-in site local)))
                     (not (stringp (oref pkg :location)))
                     (or (null usedp)
                         (and (not (null (oref pkg :owners)))
                              (cfgl-package-enabledp pkg)
                              (not (oref pkg :excluded)))))))))

(defun configuration-layer/initialize ()
  "Initialize `package.el'."
  (setq configuration-layer--refresh-package-timeout dotspacemacs-elpa-timeout)
  (unless package--initialized
    (setq package-archives (configuration-layer//resolve-package-archives
                            configuration-layer--elpa-archives))
    ;; optimization, no need to activate all the packages so early
    (setq package-enable-at-startup nil)
    (package-initialize 'noactivate)
    (require 'cask "/usr/local/Cellar/cask/0.7.4/cask.el")
    (cask-initialize)
    (require 'pallet)
    (pallet-mode t)))

(defun configuration-layer/sync (&optional no-install)
  "Synchronize declared layers in dotfile with spacemacs.
If NO-INSTALL is non nil then install steps are skipped."
  (dotspacemacs|call-func dotspacemacs/layers "Calling dotfile layers...")
  ;; (when (spacemacs-buffer//choose-banner)
  ;;   (spacemacs-buffer//inject-version))
  ;; first, declare used layers then packages as soon as possible to resolve
  ;; usage and ownership
  (configuration-layer/discover-layers)
  (configuration-layer//declare-used-layers dotspacemacs-configuration-layers)
  (configuration-layer//declare-packages configuration-layer--used-layers)
  ;; then load the functions and finally configure the layers
  (configuration-layer//load-layers-files configuration-layer--used-layers
                                           '("funcs.el"))
  (configuration-layer//configure-layers configuration-layer--used-layers)
  ;; (configuration-layer//configure-layers configuration-layer--used-layers)
  ;; pre-filter some packages to save some time later in the loading process
  (setq configuration-layer--used-distant-packages
         (configuration-layer//get-distant-packages
          configuration-layer--used-packages t))
  ;; install/uninstall packages
  ;;(configuration-layer/load-auto-layer-file)
  (unless no-install
     (if (eq 'all dotspacemacs-download-packages)
         (configuration-layer//install-packages
          (configuration-layer//get-distant-packages
           (configuration-layer/make-all-packages 'used) nil))
       (configuration-layer//install-packages
        (configuration-layer/filter-objects
         configuration-layer--used-distant-packages
         (lambda (x)
           (let ((pkg (configuration-layer/get-package x)))
             (not (oref pkg :lazy-install)))))))
     (configuration-layer//configure-packages configuration-layer--used-packages)
     (configuration-layer//load-layers-files configuration-layer--used-layers
                                             '("keybindings.el"))
     (when (and (eq 'used dotspacemacs-download-packages)
                (not configuration-layer-force-distribution)
                (not configuration-layer-exclude-all-layers))
       ;;(configuration-layer/delete-orphan-packages
        ;;configuration-layer--used-distant-packages)
       )))

(defun configuration-layer//install-packages (packages)
  "Install PACKAGES which are not lazy installed."
  (interactive)
  ;; Force the display of warning buffers at the bottom
  (message "%s" packages)
  (let ((display-buffer-alist
         '(("\\(\\*Compile-Log\\*\\)\\|\\(\\*Warnings\\*\\)"
            (display-buffer-in-side-window)
            (inhibit-same-window . t)
            (side . bottom)
            (window-height . 0.2)))))
    ;; ensure we have quelpa available first
    ;;(configuration-layer//install-quelpa)
    ;;得到那些没有安装
    (let* ((upkg-names (configuration-layer//get-uninstalled-packages packages))
           (not-inst-count (length upkg-names))
           installed-count)
      ;; installation

      (when upkg-names
	(message "%s====" (car upkg-names))
        (spacemacs-buffer/append
         (format "Found %s new package(s) to install...\n"
                 not-inst-count))
        (configuration-layer/retrieve-package-archives)
        (setq installed-count 0)
        (spacemacs//redisplay)
        (dolist (pkg-name upkg-names)
          (setq installed-count (1+ installed-count))
          (configuration-layer//install-package
           (configuration-layer/get-package pkg-name)))
        (spacemacs-buffer/append "\n")))))

(defun configuration-layer//install-package (pkg)
  "Unconditionally install the package PKG."
  (let* ((layer (when pkg (car (oref pkg :owners))))
         (location (when pkg (oref pkg :location)))
         (min-version (when pkg (oref pkg :min-version))))
    (spacemacs-buffer/replace-last-line
     (format "--> installing %s: %s%s... [%s/%s]"
             (if layer "package" "dependency")
             pkg-name (if layer (format "@%S" layer) "")
             installed-count not-inst-count) t)
    (spacemacs//redisplay)
    (unless (package-installed-p pkg-name min-version)
      (condition-case-unless-debug err
          (cond
           ((or (null pkg) (eq 'elpa location))
            (configuration-layer//install-from-elpa pkg-name)
            (when pkg (oset pkg :lazy-install nil)))
           ((and (listp location) (eq 'recipe (car location)))
            (configuration-layer//install-from-recipe pkg)
            (oset pkg :lazy-install nil))
           (t (configuration-layer//warning "Cannot install package %S."
                                        pkg-name)))
        ('error
         (configuration-layer//increment-error-count)
         (spacemacs-buffer/append
          (format (concat "\nAn error occurred while installing %s "
                          "(error: %s)\n") pkg-name err))
         (spacemacs//redisplay))))))

(defun configuration-layer//install-from-elpa (pkg-name)
  "Install PKG from ELPA."
  (if (not (assq pkg-name package-archive-contents))
      (spacemacs-buffer/append
       (format (concat "\nPackage %s is unavailable. "
                       "Is the package name misspelled?\n")
               pkg-name))
    (let ((pkg-desc (assq pkg-name package-archive-contents)))
      (dolist
          (dep (configuration-layer//get-package-deps-from-archive
                pkg-name))
        (if (package-installed-p (car dep) (cadr dep))
            (configuration-layer//activate-package (car dep))
          (configuration-layer//install-from-elpa (car dep))))
      (if pkg-desc
          (package-install (cadr pkg-desc))
        (package-install pkg-name)))))

(defun configuration-layer//get-package-directory (pkg-name)
  "Return the directory path for package with name PKG-NAME."
  (let ((pkg-desc (assq pkg-name package-alist)))
    (package-desc-dir (cadr pkg-desc))))

(defun configuration-layer//activate-package (pkg)
  "Activate PKG."
  (unless (memq pkg package-activated-list)
    (package-activate pkg)))

(defun configuration-layer//get-package-deps-from-alist (pkg-name)
  "Return the dependencies alist for package with name PKG-NAME."
  (let ((pkg-desc (assq pkg-name package-alist)))
    (when pkg-desc (package-desc-reqs (cadr pkg-desc)))))

(defun configuration-layer//get-package-deps-from-archive (pkg-name)
  "Return the dependencies alist for a PKG-NAME from the archive data."
  (let* ((pkg-arch (assq pkg-name package-archive-contents))
         (reqs (when pkg-arch (package-desc-reqs (cadr pkg-arch)))))
    ;; recursively get the requirements of reqs
    (dolist (req reqs)
      (let* ((pkg-name2 (car req))
             (reqs2 (configuration-layer//get-package-deps-from-archive
                     pkg-name2)))
        (when reqs2 (setq reqs (append reqs2 reqs)))))
    reqs))

(defun configuration-layer//get-package-version-string (pkg-name)
  "Return the version string for package with name PKG-NAME."
  (let ((pkg-desc (assq pkg-name package-alist)))
    (when pkg-desc
      (package-version-join (package-desc-version (cadr pkg-desc))))))

(defun configuration-layer//get-package-version (pkg-name)
  "Return the version list for package with name PKG-NAME."
  (let ((version-string (configuration-layer//get-package-version-string
                         pkg-name)))
    (unless (string-empty-p version-string)
      (version-to-list version-string))))

(defun configuration-layer//get-latest-package-version-string (pkg-name)
  "Return the version string for package with name PKG-NAME."
  (let ((pkg-arch (assq pkg-name package-archive-contents)))
    (when pkg-arch
      (package-version-join (package-desc-version (cadr pkg-arch))))))

(defun configuration-layer//get-latest-package-version (pkg-name)
  "Return the versio list for package with name PKG-NAME."
  (let ((version-string
         (configuration-layer//get-latest-package-version-string pkg-name)))
    (unless (string-empty-p version-string)
      (version-to-list version-string))))

(defun configuration-layer//filter-packages-with-deps
    (pkg-names filter &optional use-archive)
  "Return a filtered PKG-NAMES list where each elements satisfies FILTER."
  (when pkg-names
    (let (result)
      (dolist (pkg-name pkg-names)
	(message "%s" pkg-name)
        ;; recursively check dependencies
        (let* ((deps
                (if use-archive
                    (configuration-layer//get-package-deps-from-archive
                     pkg-name)
                  (configuration-layer//get-package-deps-from-alist pkg-name)))
               (install-deps
                (when deps (configuration-layer//filter-packages-with-deps
                            (mapcar 'car deps) filter))))
          (when install-deps
            (setq result (append install-deps result))))
        (when (funcall filter pkg-name)
          (add-to-list 'result pkg-name t)))
      (delete-dups result))))

(defun configuration-layer//get-uninstalled-packages (pkg-names)
  "Return a filtered list of PKG-NAMES to install."
  (message "%s" pkg-names)
  (configuration-layer//filter-packages-with-deps
   pkg-names (lambda (x)
               (let* ((pkg (configuration-layer/get-package x))
                      (min-version (when pkg (oref pkg :min-version))))
                 (not (package-installed-p x min-version))))))

(defun configuration-layer/delete-orphan-packages (packages)
  "Delete PACKAGES if they are orphan."
  (interactive)
  (let* ((dependencies (configuration-layer//get-packages-dependencies))
         (implicit-packages (configuration-layer//get-implicit-packages
                             packages))
         (orphans (configuration-layer//get-orphan-packages
                   packages
                   implicit-packages
                   dependencies))
         (orphans-count (length orphans))
         deleted-count)
    ;; (message "dependencies: %s" dependencies)
    ;; (message "implicit: %s" implicit-packages)
    ;; (message "orphans: %s" orphans)
    (if orphans
        (progn
          (spacemacs-buffer/append
           (format "Found %s orphan package(s) to delete...\n"
                   orphans-count))
          (setq deleted-count 0)
          (dolist (orphan orphans)
            (setq deleted-count (1+ deleted-count))
            (spacemacs-buffer/replace-last-line
             (format "--> deleting %s... [%s/%s]"
                     orphan
                     deleted-count
                     orphans-count) t)
            (configuration-layer//package-delete orphan)
            (spacemacs//redisplay))
          (spacemacs-buffer/append "\n"))
      (spacemacs-buffer/message "No orphan package to delete."))))

(defun configuration-layer//package-delete (pkg-name)
  "Delete package with name PKG-NAME."
  (cond
   ((version<= "25.0.50" emacs-version)
    (let ((p (cadr (assq pkg-name package-alist))))
      ;; add force flag to ignore dependency checks in Emacs25
      (when p (package-delete p t t))))
   (t (let ((p (cadr (assq pkg-name package-alist))))
        (when p (package-delete p))))))

(defun configuration-layer//get-implicit-packages (packages)
  "Returns packages in `packages-alist' which are not found in PACKAGES."
  (let (imp-pkgs)
    (dolist (pkg package-alist)
      (let ((pkg-sym (car pkg)))
        (unless (memq pkg-sym packages)
          (add-to-list 'imp-pkgs pkg-sym))))
    imp-pkgs))
(defun configuration-layer//get-orphan-packages
    (dist-pkgs implicit-pkgs dependencies)
  "Return orphan packages."
  (let (result)
    (dolist (imp-pkg implicit-pkgs)
      (when (configuration-layer//is-package-orphan
             imp-pkg dist-pkgs dependencies)
        (add-to-list 'result imp-pkg)))
    result))

(defun configuration-layer//get-packages-dependencies ()
  "Returns dependencies hash map for all packages in `package-alist'."
  (let ((result (make-hash-table :size 512)))
    (dolist (pkg package-alist)
      (let* ((pkg-sym (car pkg))
             (deps (configuration-layer//get-package-deps-from-alist pkg-sym)))
        (dolist (dep deps)
          (let* ((dep-sym (car dep))
                 (value (ht-get result dep-sym)))
            (puthash dep-sym
                     (if value (add-to-list 'value pkg-sym) (list pkg-sym))
                     result)))))
    result))

(defun configuration-layer//declare-packages (layers)
  "Declare packages contained in LAYERS."
  (setq configuration-layer--used-packages nil)
  (let* ((warning-minimum-level :error))
    ;; first pass
    (configuration-layer/make-packages layers t t)
    ;; second pass to resolve package-usedp calls
    (configuration-layer/make-packages configuration-layer--delayed-layers t)
    (setq configuration-layer--used-packages
          (configuration-layer//sort-packages
           configuration-layer--used-packages))))


(defun configuration-layer/discover-layers ()
  "Initialize `configuration-layer--indexed-layers' with layer directories."
  ;; load private layers at the end on purpose we asume that the user layers
  ;; must have the final word on configuration choices. Let
  ;; `dotspacemacs-directory' override the private directory if it exists.
  (setq  configuration-layer--indexed-layers (make-hash-table :size 1024))
  (let ((search-paths (append (list configuration-layer-directory)
                              dotspacemacs-configuration-layer-path
                              ;; (list configuration-layer-private-layer-directory)
                              (when dotspacemacs-directory
                                (list dotspacemacs-directory))))
        (discovered '()))
    ;; depth-first search of subdirectories
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
                      ;; the same layer may have been discovered twice,
                      ;; in which case we don't need a warning
                      (unless (string-equal (oref indexed-layer :dir) sub)
                        (configuration-layer//warning
                         (concat
                          "Duplicated layer %s detected in directory \"%s\", "
                          "replacing old directory \"%s\" with new directory.")
                         layer-name-str sub (oref indexed-layer :dir))
                        (oset indexed-layer :dir sub))
                    (spacemacs-buffer/message
                     "-> Discovered configuration layer: %s" layer-name-str)
                    (configuration-layer//add-layer
                     (configuration-layer/make-layer layer-name nil nil sub)))))
               (t
                ;; layer not found, add it to search path
                (setq search-paths (cons sub search-paths)))))))))))


(defun configuration-layer//configure-packages (packages)
  "Configure all passed PACKAGES honoring the steps order."
  ;;(setq spacemacs-loading-dots-chunk-threshold
  ;;      (/ (length configuration-layer--used-packages)
  ;;         spacemacs-loading-dots-chunk-count))
  (spacemacs-buffer/message "+ Configuring bootstrap packages...")
  (configuration-layer//configure-packages-2
   (configuration-layer/filter-objects
    packages (lambda (x)
               (let ((pkg (configuration-layer/get-package x)))
                 (eq 'bootstrap (oref pkg :step))))))
  (spacemacs-buffer/message "+ Configuring pre packages...")
  (configuration-layer//configure-packages-2
   (configuration-layer/filter-objects
    packages (lambda (x)
               (let ((pkg (configuration-layer/get-package x)))
                 (eq 'pre (oref pkg :step))))))
  (spacemacs-buffer/message "+ Configuring packages...")
  (configuration-layer//configure-packages-2
   (configuration-layer/filter-objects
    packages (lambda (x)
               (let ((pkg (configuration-layer/get-package x)))
                 (null (oref pkg :step)))))))

(defun configuration-layer//configure-packages-2 (packages)
  "Configure all passed PACKAGES."
  (dolist (pkg-name packages)
    ;;(spacemacs-buffer/loading-animation)
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
        ;; load-path
        (let ((location (oref pkg :location)))
          (cond
           ((stringp location)
            (if (file-directory-p location)
                (push (file-name-as-directory location) load-path)
              (configuration-layer//warning
               "Location path for package %S does not exists (value: %s)."
               pkg location)))
           ((and (eq 'local location)
                 (eq 'dotfile (car (oref pkg :owners))))
            (push (file-name-as-directory
                   (concat configuration-layer-private-directory "local/"
                           (symbol-name (oref pkg :name))))
                  load-path))
           ((eq 'local location)
            (let* ((owner (configuration-layer/get-layer
                           (car (oref pkg :owners))))
                   (dir (when owner (oref owner :dir))))
              (push (format "%slocal/%S/" dir pkg-name) load-path)))))
        ;; configuration
        (unless (memq (oref pkg :location) '(local site built-in))
          (configuration-layer//activate-package pkg-name))
        (cond
         ((eq 'dotfile (car (oref pkg :owners)))
          (spacemacs-buffer/message
           (format "%S is configured in the dotfile." pkg-name)))
         (t
          (configuration-layer//configure-package pkg))))))))

(defun configuration-layer//configure-package (pkg)
  "Configure PKG object."
  (let* ((pkg-name (oref pkg :name))
         (owner (car (oref pkg :owners)))
         (owner-layer (configuration-layer/get-layer owner))
         (disabled-for-layers (oref owner-layer :disabled-for)))
    (spacemacs-buffer/message (format "Configuring %S..." pkg-name))
    ;; pre-init
    (mapc (lambda (layer)
            (if (memq layer disabled-for-layers)
                (spacemacs-buffer/message
                 (format "  -> ignored pre-init (%S)..." layer))
              (spacemacs-buffer/message
               (format "  -> pre-init (%S)..." layer))
              (condition-case-unless-debug err
                  (funcall (intern (format "%S/pre-init-%S" layer pkg-name)))
                ('error
                 (configuration-layer//increment-error-count)
                 (spacemacs-buffer/append
                  (format
                   (concat "\nAn error occurred while pre-configuring %S "
                           "in layer %S (error: %s)\n")
                   pkg-name layer err))))))
          (oref pkg :pre-layers))
    ;; init
    (spacemacs-buffer/message (format "  -> init (%S)..." owner))
    (funcall (intern (format "%S/init-%S" owner pkg-name)))
    ;; post-init
    (mapc (lambda (layer)
            (if (memq layer disabled-for-layers)
                (spacemacs-buffer/message
                 (format "  -> ignored post-init (%S)..." layer))
              (spacemacs-buffer/message
               (format "  -> post-init (%S)..." layer))
              (condition-case-unless-debug err
                  (funcall (intern (format "%S/post-init-%S" layer pkg-name)))
                ('error
                 (configuration-layer//increment-error-count)
                 (spacemacs-buffer/append
                  (format
                   (concat "\nAn error occurred while post-configuring %S "
                           "in layer %S (error: %s)\n")
                   pkg-name layer err))))))
          (oref pkg :post-layers))))

(defun configuration-layer//configure-layers (layer-names)
  "Configure layers with LAYER-NAMES."
  (let ((warning-minimum-level :error))
    (dolist (layer-name layer-names)
      (configuration-layer//load-layer-files layer-name '("config.el")))))

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


(defun configuration-layer/declare-used-layers (layers-specs)
  "Declare used layers with LAYERS-SPECS."
  (configuration-layer/declare-layers layers-specs 'used))

(defun configuration-layer/declare-layers (layers-specs &optional usedp)
  "Declare layers with LAYERS-SPECS."
  (mapc (lambda (x)
          (configuration-layer/declare-layer x usedp))
        layers-specs))

(defun configuration-layer/declare-used-layer (layer-specs)
  "Declare used layer with LAYER-SPECS."
  (configuration-layer/declare-layer layer-specs 'used))

(defun configuration-layer/declare-layer (layer-specs &optional usedp)
  "Declare a single layer with spec LAYER-SPECS."
  (let* ((layer-name (if (listp layer-specs) (car layer-specs) layer-specs))
         (layer (configuration-layer/get-layer layer-name)))
    (if layer
        (let ((obj (configuration-layer/make-layer
                    layer-specs
                    (configuration-layer/get-layer layer-name)
                    usedp)))
          (configuration-layer//add-layer obj usedp)
          (configuration-layer//set-layer-variables obj)
          (when usedp
            (configuration-layer//load-layer-files layer-name '("layers.el"))))
      (configuration-layer//warning "Unknown layer %s declared in dotfile."
                                    layer-name))))


(defun configuration-layer//declare-used-layers (&optional layers-specs)
  "Declare used layers from LAYERS-SPECS list.
If LAYERS-SPECS is nil then read variable `dotspacemacs-configuration-layers'."
  (when (listp layers-specs) (message "%s" (car layers-specs)))
  (setq configuration-layer--used-layers nil)
  (unless layers-specs
    (setq layers-specs dotspacemacs-configuration-layers))
  (unless configuration-layer-exclude-all-layers
    (dolist (layer-specs layers-specs)
      (let* ((layer-name (if (listp layer-specs) (car layer-specs) layer-specs))
             (layer (configuration-layer/get-layer layer-name)))
        (if layer
            (let ((layer-path (oref layer :dir)))
              (unless (string-match-p "+distributions" layer-path)
                (configuration-layer/declare-used-layer layer-specs)))
          (configuration-layer//warning "Unknown layer %s declared in dotfile."
                                        layer-name))))
    (setq configuration-layer--used-layers
          (reverse configuration-layer--used-layers)))
  ;; distribution and bootstrap layers are always first
   (let ((distribution (if configuration-layer-force-distribution
                           configuration-layer-force-distribution
                         dotspacemacs-distribution)))
     (unless (eq 'spacemacs-bootstrap distribution)
       (configuration-layer/declare-used-layer distribution)))
   (configuration-layer/declare-used-layer 'spacemacs-bootstrap)
  )
(defun configuration-layer//select-packages (layer-specs packages)
  "Return the selected packages of LAYER-SPECS from given PACKAGES list."
  (let* ((value (when (listp layer-specs)
                  (spacemacs/mplist-get layer-specs :packages)))
         (selected-packages (if (and (not (null (car value)))
                                     (listp (car value)))
                                (car value)
                              value)))
    (cond
     ;; select packages
     ((and selected-packages
           (not (memq (car selected-packages) '(all not))))
      selected-packages)
     ;; unselect packages
     ((and selected-packages
           (eq 'not (car selected-packages)))
      (delq nil (mapcar (lambda (x)
                          (let ((pkg-name (if (listp x) (car x) x)))
                            (unless (memq pkg-name selected-packages)
                              pkg-name)))
                        packages)))
     ;; no package selections or all package selected
     (t 'all))))

(defun configuration-layer//set-layers-variables (layers)
  "Set the configuration variables for the passed LAYERS."
  (mapc 'configuration-layer//set-layer-variables layers))

(defun configuration-layer//set-layer-variables (layer)
  "Set the configuration variables for the passed LAYER."
  (let ((variables (oref layer :variables)))
    (while variables
      (let ((var (pop variables)))
        (if (consp variables)
            (condition-case-unless-debug err
                (set-default var (eval (pop variables)))
              ('error
               (configuration-layer//increment-error-count)
               (spacemacs-buffer/append
                (format (concat "\nAn error occurred while setting layer "
                                "variable %s "
                                "(error: %s). Be sure to quote the value "
                                "if needed.\n") var err))))
          (configuration-layer//warning "Missing value for variable %s !"
                                    var))))))

(defun configuration-layer//load-layers-files (layer-names files)
  "Load the files of list FILES for all passed LAYER-NAMES."
  (dolist (layer-name layer-names)
    (configuration-layer//load-layer-files layer-name files)))

(defun configuration-layer//load-layer-files (layer-name files)
  "Load the files of list FILES for the layer with the given LAYER-NAME."
  (let ((obj (configuration-layer/get-layer layer-name)))
    (when obj
      (dolist (file files)
        (let ((file (concat (oref obj :dir) file)))
          (if (file-exists-p file) (load file)))))))


(defun configuration-layer//resolve-package-archives (archives)
  "Resolve HTTP handlers for each archive in ARCHIVES and return a list
of all reachable ones.
If the address of an archive already contains the protocol then this address is
left untouched.
The returned list has a `package-archives' compliant format."
  (mapcar
   (lambda (x)
     (cons (car x)
           (if (string-match-p "http" (cdr x))
               (cdr x)
             (concat (if (and dotspacemacs-elpa-https
                              (not spacemacs-insecure)
                              ;; for now org ELPA repository does
                              ;; not support HTTPS
                              ;; TODO when org ELPA repo support
                              ;; HTTPS remove the check
                              ;; `(not (equal "org" (car x)))'
                              (not (equal "org" (car x))))
                         "https://"
                       "http://") (cdr x)))))
   archives))


(defun configuration-layer//warning (msg &rest args)
  "Display MSG as a warning message in buffer `*Messages*'.
If `configuration-layer--inhibit-warnings' is non nil then this function is a
no-op."
  (unless configuration-layer--inhibit-warnings
    (apply 'spacemacs-buffer/warning msg args)))

(defun configuration-layer//sort-packages (packages)
  "Return a sorted list of PACKAGES objects."
  (sort packages (lambda (x y) (string< (symbol-name x) (symbol-name y)))))

(defun configuration-layer/filter-objects (objects ffunc)
  "Return a filtered OBJECTS list where each element satisfies FFUNC."
  (reverse (cl-reduce (lambda (acc x)
                     (if (funcall ffunc x) (push x acc) acc))
                   objects
                   :initial-value nil)))

(defun configuration-layer/retrieve-package-archives (&optional quiet force)
  "Retrieve all archives declared in current `package-archives'.

This function first performs a simple GET request with a timeout in order to
fix very long refresh time when an archive is not reachable.

Note that this simple GET is a heuristic to determine the availability
likelihood of an archive, so it can gives false positive if the archive
page is served but the archive is not.

If QUIET is non nil then the function does not print message in the Spacemacs
home buffer.

If FORCE is non nil then refresh the archives even if they have been already
refreshed during the current session."
  (unless (and configuration-layer--package-archives-refreshed
               (not force))
    (setq configuration-layer--package-archives-refreshed t)
    (let ((count (length package-archives))
          (i 1))
      (dolist (archive package-archives)
        (unless quiet
          (spacemacs-buffer/replace-last-line
           (format "--> refreshing package archive: %s... [%s/%s]"
                   (car archive) i count) t))
        (spacemacs//redisplay)
        (setq i (1+ i))
        (unless (eq 'error
                    (with-timeout
                        (dotspacemacs-elpa-timeout
                         (progn
                           (display-warning
                            'spacemacs
                            (format
                             "\nError connection time out for %s repository!"
                             (car archive)) :warning)
                           'error))
                      (condition-case err
                          (url-retrieve-synchronously (cdr archive))
                        ('error
                         (display-warning 'spacemacs
                          (format
                           "\nError while contacting %s repository!"
                           (car archive)) :warning)
                         'error))))
          (let ((package-archives (list archive)))
            (package-refresh-contents))))
      (package-read-all-archive-contents)
      (unless quiet (spacemacs-buffer/append "\n")))))

(defun configuration-layer/get-layer-path (layer)
  "Return the path for LAYER symbol."
  (let ((obj (ht-get configuration-layer--indexed-layers layer)))
    (when obj (oref obj :dir))))

(defun configuration-layer/package-usedp (name)
  "Return non-nil if NAME is the name of a used package."
  (let ((obj (configuration-layer/get-package name)))
    (and obj (cfgl-package-get-safe-owner obj)
         (not (oref obj :excluded)))))

(defun configuration-layer/layer-usedp (layer-name)
  "Return non-nil if LAYER-NAME is the name of a used layer."
  (let ((obj (configuration-layer/get-layer layer-name)))
    (when obj (memq layer-name configuration-layer--used-layers))))

(defun configuration-layer//get-package-deps-from-alist (pkg-name)
  "Return the dependencies alist for package with name PKG-NAME."
  (let ((pkg-desc (assq pkg-name package-alist)))
    (when pkg-desc (package-desc-reqs (cadr pkg-desc)))))

(defun configuration-layer//increment-error-count ()
  "Increment the error counter."
  (if configuration-layer-error-count
      (setq configuration-layer-error-count
            (1+ configuration-layer-error-count))
    (setq configuration-layer-error-count 1)))
(defun configuration-layer/configured-packages-stats (packages)
  "Return a statistics alist regarding the number of configured PACKAGES."
  `((total ,(length packages))
    (elpa ,(length (configuration-layer/filter-objects
                    packages
                    (lambda (x)
                      (let ((pkg (configuration-layer/get-package x)))
                        (eq 'elpa (oref pkg :location)))))))
    (recipe ,(length (configuration-layer/filter-objects
                      packages
                      (lambda (x)
                        (let* ((pkg (configuration-layer/get-package x))
                               (location (oref pkg :location)))
                          (and (listp location)
                               (eq 'recipe (car location))))))))
    (local ,(length (configuration-layer/filter-objects
                     packages
                     (lambda (x)
                       (let ((pkg (configuration-layer/get-package x)))
                         (memq (oref pkg :location) '(local site)))))))
    (built-in ,(length (configuration-layer/filter-objects
                        packages
                        (lambda (x)
                          (let ((pkg (configuration-layer/get-package x)))
                            (eq 'built-in (oref pkg :location)))))))))

(defun configuration-layer/display-summary (start-time)
  "Display a summary of loading time."
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time)))
        (stats (configuration-layer/configured-packages-stats
                configuration-layer--used-packages)))
    (spacemacs-buffer/append
     (format "\n%s packages loaded in %.3fs (e:%s r:%s l:%s b:%s)\n"
             (cadr (assq 'total stats))
             elapsed
             (cadr (assq 'elpa stats))
             (cadr (assq 'recipe stats))
             (cadr (assq 'local stats))
             (cadr (assq 'built-in stats))))))



(defun configuration-layer/get-layers-list ()
  "Return a list of all discovered layer symbols."
  (ht-keys configuration-layer--indexed-layers))
(defun configuration-layer/get-layer-local-dir (layer)
  "Return the value of SLOT for the given LAYER."
  (let ((obj (ht-get configuration-layer--indexed-layers layer)))
    (when obj (concat (oref obj :dir) "local/"))))

(defun configuration-layer/get-layer-path (layer)
  "Return the path for LAYER symbol."
  (let ((obj (ht-get configuration-layer--indexed-layers layer)))
    (when obj (oref obj :dir))))
(defun configuration-layer/make-all-packages (&optional usedp)
  "Create objects for _all_ packages.
USEDP if non-nil indicates that made packages are used packages."
  (configuration-layer/make-packages (configuration-layer/get-layers-list)
                                     usedp))

(provide 'core-configuration-layer)
