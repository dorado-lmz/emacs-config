
(defvar spacemacs--diminished-minor-modes nil
  "List of diminished modes to unicode or ascii values.")

(defmacro spacemacs|diminish (mode &optional unicode ascii)
  "Diminish MODE name in mode line to UNICODE or ASCII depending on the value
`dotspacemacs-mode-line-unicode-symbols'.
if ASCII is not provided then UNICODE is used instead. If neither are provided,
the mode will not show in the mode line."
  `(let ((cell (assq ',mode spacemacs--diminished-minor-modes)))
     (if cell
	 (setcdr cell '(,unicode ,ascii))
       (push '(,mode ,unicode ,ascii) spacemacs--diminished-minor-modes))))

(defmacro spacemacs|hide-lighter (mode)
  "Diminish MODE name in mode line to LIGHTER."
  `(eval-after-load 'diminish '(diminish ',mode)))
(provide 'core-fonts-support)
