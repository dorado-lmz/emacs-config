(require 'cl)

(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)
(global-hungry-delete-mode)

(require 'smartparens-config)
(smartparens-global-mode t)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

(global-evil-leader-mode)
(setq evil-leader/leader "<SPC>")
(evil-mode 1)
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)


(evil-leader/set-key
  "ff" 'find-file
  "fr" 'recentf-open-files
  "fj" 'dired-jump
  "bb" 'switch-to-buffer
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
  "feR" 'dotspacemacs/sync-configuration-layers)   

(window-numbering-mode 1)
(require 'powerline)
(powerline-default-theme)

(setq auto-mode-alist
      (append
       '(
	 ("\\.js\\'" . js2-mode)
	 ("\\.html\\'" . web-mode)
	 ("\\.cl\\'" . lisp-mode)
	 )
       auto-mode-alist)) 

(require 'evil-surround)
(global-evil-surround-mode t)

(evilnc-default-hotkeys)
(define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
(define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)

(which-key-mode t)
(setq which-key-side-window-location 'right)
(provide 'init-package)

