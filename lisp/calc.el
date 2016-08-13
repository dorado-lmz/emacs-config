(setq major-mode 'calc-mode)

(setq mode-name "Calculator")


;;define local key map
(defvar calc-mode-map (make-sparse-keymap))

;;ralate key map to mode
(use-local-map calc-mode-map)

(provide 'calc-mode)
