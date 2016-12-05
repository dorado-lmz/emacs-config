(setq ivy-packages
      '(
       counsel
       ivy
        ))


(defun ivy/init-counsel ()

  (use-package counsel
    :config
    (progn
      (spacemacs/set-leader-keys
        "ff"  'counsel-find-file
        ;; jump
        ;; register/ring
        "ry"  'counsel-yank-pop
        ;; jumping
        "sj"  'counsel-imenu
        )
      )
  ))

(defun ivy/init-ivy ()
  (use-package ivy
               :config
               (progn
                 (with-eval-after-load 'recentf
		   (setq ivy-use-virtual-buffers t))
		 (ivy-mode t)
                 (spacemacs/set-leader-keys
                   "fr" 'counsel-recentf
                   "bb" 'ivy-switch-buffer))))
