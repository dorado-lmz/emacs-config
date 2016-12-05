(defconst spacemacs-buffer-name "*log*"
  "The name of the spacemacs buffer.")


(defun spacemacs-buffer/message (msg &rest args)
  "Display MSG in message prepended with '(Spacemacs)'.
The message is displayed only if `init-file-debug' is non nil."
  (when init-file-debug
    (spacemacs-buffer/append (concat "[->Spacemacs<-] %s" (apply 'format msg args)))
    ))

(defun spacemacs-buffer/warning (msg &rest args)
    (spacemacs-buffer/append (concat "[->Spacemacs Warning<-] %s" (apply 'format msg args)))
  )

(defun spacemacs-buffer/append (msg)
  (with-current-buffer (get-buffer-create spacemacs-buffer-name)
      (goto-char (point-max))
      (let ((buffer-read-only nil))
	(insert (concat msg "\n")))))

(provide 'util-log)
