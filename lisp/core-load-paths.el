;; paths
(defconst user-home-directory
          (expand-file-name "~/")
          "User home directory (~/).")

(defvar spacemacs-start-directory
  user-emacs-directory
  "Spacemacs start directory.")

(defun add-to-load-path (dir) (add-to-list 'load-path dir))

(defun add-to-load-path-if-exists (dir)
  "If DIR exists in the file system, add it to `load-path'."
  (when (file-exists-p dir)
      (add-to-load-path dir)))

