
(defun add-to-load-path (dir) (add-to-list 'load-path dir))

(defun add-to-load-path-if-exists (dir)
  "If DIR exists in the file system, add it to `load-path'."
  (when (file-exists-p dir)
      (add-to-load-path dir)))


(defconst user-home-directory
  (expand-file-name "~/")
  "User home directory (~/).")

;; paths
(defvar spacemacs-start-directory
  user-emacs-directory
  "Spacemacs start directory.")
(defconst spacemacs-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "Spacemacs storage area for persistent files")
(defconst spacemacs-auto-save-directory
  (expand-file-name (concat spacemacs-cache-directory "auto-save/"))
  "Spacemacs auto-save directory")

;; load paths
(mapc 'add-to-load-path
      `(
        ,(concat spacemacs-start-directory "core/")
        ,(concat spacemacs-start-directory "core/libs/")
        ,(concat spacemacs-start-directory "core/aprilfool/")
        ))


