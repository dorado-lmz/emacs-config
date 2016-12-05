(defun open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-my-init-file ()
  (interactive)
  (find-file "~/.emacs.d/.spacemacs"))

;;(spacemacs/set-leader-keys
;;  "ps" 'helm-do-ag-project-root
;;  )

(evil-leader/set-key
  "ff" 'counsel-find-file
  "fr" 'ivy-recentf
  ;;"fj" 'dired-jump
  "bb" 'ivy-switch-buffer
  "bd" 'kill-buffer
  "w/" 'split-window-right
  "w-" 'split-window-below
  "1" 'select-window-1
  "2" 'select-window-2
  "3" 'select-window-3
  "4" 'select-window-4
  ":" 'counsel-M-x
  "ee" 'eval-last-sexp
  "wc" 'delete-window
  "wm" 'delete-other-windows
  "si" 'counsel-imenu)

(global-set-key (kbd "C-s") 'swiper)

(global-set-key (kbd "M-x") 'counsel-M-x)


(global-set-key (kbd "<f2>") 'open-init-file)
(global-set-key (kbd "<f3>") 'open-my-init-file)

(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

(global-set-key (kbd "C-c r") 'org-capture)
(global-set-key (kbd "C-c C-r") 'ivy-recentf)

(global-set-key (kbd "C-c a") 'org-agenda)

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

(global-set-key (kbd "M-s i") 'counsel-imenu)

(global-set-key (kbd "M-s e") 'iedit-mode)

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(provide 'init-keybinding)
