(defvar spacemacs-default-map (make-sparse-keymap)
  "Base keymap for all all spacemacs leader key commands.")

(defun spacemacs/set-leader-keys (key def &rest bindings)
  "Add KEY and DEF as key bindings under
`dotspacemacs-leader-key' and `dotspacemacs-emacs-leader-key'."
  (while key
    (define-key spacemacs-default-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))

(defalias 'evil-leader/set-key 'spacemacs/set-leader-keys)
(provide 'core-keybindings)
