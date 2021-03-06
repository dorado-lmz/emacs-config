(defun open-init-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-my-init-file ()
  (interactive)
  (find-file "~/.lmzemacs.d/init.el"))

(global-set-key (kbd "<f2>") 'open-init-file)
(global-set-key (kbd "<f3>") 'open-my-init-file)

(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)


(global-set-key (kbd "C-s") 'swiper)

(global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c p f") 'counsel-git)

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

(provide 'init-keybinding)
