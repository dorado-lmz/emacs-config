(defconst spacemacs-buffer-name "*spacemacs*"
  "The name of the spacemacs buffer.")


(defun spacemacs-buffer/message (msg &rest args)
  "Display MSG in message prepended with '(Spacemacs)'.
The message is displayed only if `init-file-debug' is non nil."
  (when init-file-debug
    (message "(Spacemacs) %s" (apply 'format msg args))))

(defun spacemacs-buffer/warning (msg &rest args)
  "Display MSG as a warning message but in buffer `*Messages*'.
The message is always displayed. "
  (message "(Spacemacs) Warning: %s" (apply 'format msg args)))
(provide 'core-spacemacs-buffer)

(defun spacemacs-buffer/append (msg &optional messagebuf)
  "Append MSG to spacemacs buffer. If MESSAGEBUF is not nil then MSG is
 also written in message buffer."
  (with-current-buffer (get-buffer-create spacemacs-buffer-name)
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg)
      (if messagebuf (message "(Spacemacs) %s" msg)))
    (spacemacs-buffer/set-mode-line "")))

(defun spacemacs-buffer/set-mode-line (format)
  "Set mode-line format for spacemacs buffer."
  (with-current-buffer (get-buffer-create spacemacs-buffer-name)
    (setq mode-line-format format)))

(defun spacemacs-buffer/replace-last-line (msg &optional messagebuf)
  "Replace the last line of the spacemacs buffer with MSG. If MESSAGEBUF is
 not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create spacemacs-buffer-name)
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (delete-region (line-beginning-position) (point-max))
      (insert msg)
      (if messagebuf (message "(Spacemacs) %s" msg)))
    (spacemacs-buffer/set-mode-line "")))

