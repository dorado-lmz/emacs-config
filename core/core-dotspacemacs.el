(defconst dotspacemacs-test-results-buffer "*dotfile-test-results*"
  "Name of the buffer to display dotfile test results.")

(defvar dotspacemacs-download-packages 'used
  "Defines the behaviour of Spacemacs when downloading packages.
Possible values are `used', `used-but-keep-unused' and `all'. `used' will
download only explicitly used packages and remove any unused packages as well as
their dependencies. `used-but-keep-unused' will download only the used packages
but won't delete them if they become unused. `all' will download all the
packages regardless if they are used or not and packages won't be deleted by
Spacemacs.")

(defconst dotspacemacs-directory
  (let* ((env (getenv "SPACEMACSDIR"))
         (env-dir (when env (expand-file-name (concat env "/"))))
         (no-env-dir-default (expand-file-name
                              (concat user-home-directory
                                      ".lmzemacs.d/"))))
    (cond
     ((and env (file-exists-p env-dir))
      env-dir)
     ((file-exists-p no-env-dir-default)
      no-env-dir-default)
     (t
      nil)))
  "Optional spacemacs directory, which defaults to
~/.spacemacs.d. This setting can be overridden using the
SPACEMACSDIR environment variable. If neither of these
directories exist, this variable will be nil.")



;; if .spacemacs exit,use it.if not,use .spacemacs.d/init.el
(defvar dotspacemacs-filepath
  (let* ((default (concat user-home-directory ".spacemacs"))
         (spacemacs-dir-init (when dotspacemacs-directory
                                 (concat dotspacemacs-directory
                                         "init.el"))))
    (if (and (not (file-exists-p default))
             dotspacemacs-directory
             (file-exists-p spacemacs-dir-init))
        spacemacs-dir-init
      default))
  "Filepath to the installed dotfile. If ~/.spacemacs exists,
then this is used. If ~/.spacemacs does not exist, then check
for init.el in dotspacemacs-directory and use this if it
exists. Otherwise, fallback to ~/.spacemacs")


(defvar dotspacemacs-configuration-layers '(emacs-lisp)
  "List of configuration layers to load.")

(defvar dotspacemacs-elpa-timeout 5
  "Maximum allowed time in seconds to contact an ELPA repository.")

(defun dotspacemacs/location ()
  "Return the absolute path to the spacemacs dotfile."
  dotspacemacs-filepath)


(defvar dotspacemacs-default-font '("Source Code Pro"
                                    :size 10
                                    :weight normal
                                    :width normal
                                    :powerline-scale 1.1)
  "Default font, or prioritized list of fonts. `powerline-scale'
allows to quickly tweak the mode-line size to make separators
look not too crappy.

Has no effect when running Emacs in terminal.")


(defmacro dotspacemacs|call-func (func &optional msg)
  "Call the function from the dotfile only if it is bound.
If MSG is not nil then display a message in `*Messages'. Errors
are caught and signalled to user in spacemacs buffer."
  `(progn
     ;; (when ,msg (spacemacs-buffer/message ,msg))
     (when (fboundp ',func)
       (condition-case-unless-debug err
           (,func)
         (error
          (configuration-layer//increment-error-count)
          (spacemacs-buffer/append (format "Error in %s: %s\n"
                                           ',(symbol-name func)
                                           (error-message-string err))
                                   t))))))

(defun dotspacemacs/sync-configuration-layers (&optional arg)
  "Synchronize declared layers in dotfile with spacemacs.

Called with `C-u' skips `dotspacemacs/user-config'.
Called with `C-u C-u' skips `dotspacemacs/user-config' _and_ preleminary tests."
  (interactive "P")
  (when (file-exists-p dotspacemacs-filepath)
    (with-current-buffer (find-file-noselect dotspacemacs-filepath)
      (let ((dotspacemacs-loading-progress-bar nil))
        (setq spacemacs-loading-string "")
        (save-buffer)
        (let ((tests-ok (or (equal arg '(16)) (dotspacemacs/test-dotfile t))))
          (if tests-ok
              (progn
                (load-file buffer-file-name)
                (dotspacemacs|call-func dotspacemacs/init
                                        "Calling dotfile init...")
                (dotspacemacs|call-func dotspacemacs/user-init
                                        "Calling dotfile user init...")
                ;; (setq dotspacemacs-editing-style
                ;;       (dotspacemacs//read-editing-style-config
                ;;        dotspacemacs-editing-style))
                (configuration-layer/sync)
                (if (member arg '((4) (16)))
                    (message (concat "Done (`dotspacemacs/user-config' "
                                     "function has been skipped)."))
                  (dotspacemacs|call-func dotspacemacs/user-config
                                          "Calling dotfile user config...")
                  (run-hooks 'spacemacs-post-user-config-hook)
                  (message "Done.")))
            (switch-to-buffer-other-window dotspacemacs-test-results-buffer)
            (spacemacs-buffer/warning "Some tests failed, check `%s' buffer"
                                      dotspacemacs-test-results-buffer))))))
  (when (configuration-layer/package-usedp 'spaceline)
    (spacemacs//set-powerline-for-startup-buffers)))

(defun dotspacemacs/test-dotfile (&optional hide-buffer)
  "Test settings in dotfile for correctness.
 Return non-nil if all the tests passed."
  (interactive)
  (configuration-layer/discover-layers)
  (let ((min-version "0.0"))
    ;; dotspacemacs-version not implemented yet
    ;; (if (version< dotspacemacs-version min-version)
    (if nil
        (error (format (concat "error: dotspacemacs/test-dotfile requires "
                               "dotspacemacs-version %s") min-version))
      (with-current-buffer (get-buffer-create dotspacemacs-test-results-buffer)
        (unless hide-buffer
          (switch-to-buffer-other-window dotspacemacs-test-results-buffer))
        (org-mode)
        (org-indent-mode)
        (view-mode)
        (when (bound-and-true-p flyspell-mode)
          (flyspell-mode -1))
        (let (buffer-read-only)
          (erase-buffer)
          (insert (format "* Running tests on [[file:%s][%s]] (v%s)\n"
                          dotspacemacs-filepath dotspacemacs-filepath "0.0"))
          ;; dotspacemacs-version not implemented yet
          ;; (insert (format "* Running tests on %s (v%s)\n" dotspacemacs-filepath dotspacemacs-version))
          (prog1
              ;; execute all tests no matter what
              (cl-reduce (lambda (x y)
                        (and (funcall y) x))
                      '(dotspacemacs//test-dotspacemacs/layers
                        dotspacemacs//test-dotspacemacs/init)
                      :initial-value t)
            (goto-char (point-min))))))))

(defvar dotspacemacs-elpa-https t
  "If non nil ELPA repositories are contacted via HTTPS whenever it's
possible. Set it to nil if you have no way to use HTTPS in your
environment, otherwise it is strongly recommended to let it set to t.")

;;load dotfile,called at core/core-spacemacs.el
(defun dotspacemacs/load-file ()
  "Load ~/.spacemacs if it exists."
  (let ((dotspacemacs (dotspacemacs/location)))
    (if (file-exists-p dotspacemacs)
        (with-demoted-errors "Error loading .spacemacs: %S" (load dotspacemacs))
      (message "dotspacemacs not exits!"))))

(defun dotspacemacs//test-dotspacemacs/layers ()
  "Tests for `dotspacemacs/layers'"
  (insert
   (format (concat "\n* Testing settings in dotspacemacs/layers "
                   "[[file:%s::dotspacemacs/layers][Show in File]]\n")
           dotspacemacs-filepath))
  ;; protect global values of these variables
  (let (dotspacemacs-configuration-layer-path dotspacemacs-configuration-layers
        dotspacemacs-additional-packages dotspacemacs-excluded-packages
        dotspacemacs-download-packages
        (passed-tests 0) (total-tests 0))
    (load dotspacemacs-filepath)
    (dotspacemacs/layers)
    (spacemacs//test-list
     'stringp 'dotspacemacs-configuration-layer-path
     "is a string" "path")
    (spacemacs//test-list
     'file-directory-p 'dotspacemacs-configuration-layer-path
     "exists in filesystem" "path")
    (setq dotspacemacs-configuration-layers
          (mapcar (lambda (l) (if (listp l) (car l) l))
                  dotspacemacs-configuration-layers))
    (spacemacs//test-list
     'configuration-layer/get-layer-path
     'dotspacemacs-configuration-layers  "can be found" "layer")
    (insert (format
             (concat "** RESULTS: "
                     "[[file:%s::dotspacemacs/layers][dotspacemacs/layers]] "
                     "passed %s out of %s tests\n")
             dotspacemacs-filepath passed-tests total-tests))
    (equal passed-tests total-tests)))

(defun dotspacemacs/get-variable-string-list ()
  "Return a list of all the dotspacemacs variables as strings."
  (all-completions "" obarray
                   (lambda (x)
                     (and (boundp x)
                          (not (keywordp x))
                          (string-prefix-p "dotspacemacs"
                                           (symbol-name x))))))

(defun dotspacemacs/get-variable-list ()
  "Return a list of all dotspacemacs variable symbols."
  (mapcar 'intern (dotspacemacs/get-variable-string-list)))


(defmacro dotspacemacs||let-init-test (&rest body)
  "Macro to protect dotspacemacs variables"
  `(let ((fpath dotspacemacs-filepath)
         ,@(dotspacemacs/get-variable-list)
         (passed-tests 0) (total-tests 0))
     (setq dotspacemacs-filepath fpath)
     (load dotspacemacs-filepath)
     ,@body))

(defun dotspacemacs//test-dotspacemacs/init ()
  "Tests for `dotspacemacs/init'"
  (insert
   (format (concat "\n* Testing settings in dotspacemacs/init "
                   "[[file:%s::dotspacemacs/init][Show in File]]\n")
           dotspacemacs-filepath))
  (dotspacemacs||let-init-test
   (dotspacemacs/init)
   (spacemacs//test-var
    (lambda (x) (or (member x '(vim emacs hybrid))
                    (and (listp x)
                         (spacemacs/mplist-get x :variables))))
    'dotspacemacs-editing-style
    "is \'vim, \'emacs or \'hybrid or and list with `:variable' keyword")
   (spacemacs//test-var
    (lambda (x) (member x '(original cache nil)))
    'dotspacemacs-auto-save-file-location (concat "is one of \'original, "
                                                  "\'cache or nil"))
   (spacemacs//test-var
    (lambda (x) (member x '(all any current nil)))
    'dotspacemacs-highlight-delimiters "is one of \'all, \'any, \'current or nil")
   (spacemacs//test-list
    (lambda (x)
      (let ((el (or (car-safe x) x))
            (list-size (cdr-safe x)))
      (member el '(recents bookmarks projects todos agenda))))
    'dotspacemacs-startup-lists (concat "includes \'recents, "
                              "\'bookmarks, \'todos, "
                              "\'agenda or \'projects"))
   (spacemacs//test-list
    (lambda (x)
      (let ((el (or (car-safe x) x))
            (list-size (cdr-safe x)))
        (or (null list-size)(numberp list-size))))
    'dotspacemacs-startup-lists (concat "list size is a number"))
   (spacemacs//test-var 'stringp 'dotspacemacs-leader-key "is a string")
   (spacemacs//test-var 'stringp 'dotspacemacs-emacs-leader-key "is a string")
   (spacemacs//test-var
    (lambda (x) (or (null x) (stringp x)))
    'dotspacemacs-major-mode-leader-key "is a string or nil")
   (spacemacs//test-var
    (lambda (x) (or (null x) (stringp x)))
    'dotspacemacs-major-mode-emacs-leader-key "is a string or nil")
   (spacemacs//test-var 'stringp 'dotspacemacs-emacs-command-key "is a string")
   (insert (format
            (concat "** RESULTS: "
                    "[[file:%s::dotspacemacs/init][dotspacemacs/init]] "
                    "passed %s out of %s tests\n")
            dotspacemacs-filepath passed-tests total-tests))
   (equal passed-tests total-tests)))

(provide 'core-dotspacemacs)
